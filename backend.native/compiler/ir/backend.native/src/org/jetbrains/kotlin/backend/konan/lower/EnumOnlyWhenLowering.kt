package org.jetbrains.kotlin.backend.konan.lower

import org.jetbrains.kotlin.backend.common.FileLoweringPass
import org.jetbrains.kotlin.backend.common.IrElementTransformerVoidWithContext
import org.jetbrains.kotlin.backend.konan.Context
import org.jetbrains.kotlin.descriptors.ClassDescriptor
import org.jetbrains.kotlin.descriptors.ClassKind
import org.jetbrains.kotlin.incremental.components.NoLookupLocation
import org.jetbrains.kotlin.ir.declarations.*
import org.jetbrains.kotlin.ir.declarations.impl.IrVariableImpl
import org.jetbrains.kotlin.ir.descriptors.IrTemporaryVariableDescriptorImpl
import org.jetbrains.kotlin.ir.expressions.*
import org.jetbrains.kotlin.ir.expressions.impl.IrCallImpl
import org.jetbrains.kotlin.ir.expressions.impl.IrConstImpl
import org.jetbrains.kotlin.ir.expressions.impl.IrGetValueImpl
import org.jetbrains.kotlin.ir.util.getArguments
import org.jetbrains.kotlin.ir.visitors.transformChildrenVoid
import org.jetbrains.kotlin.name.Name


internal class EnumOnlyWhenLowering(val context: Context) : FileLoweringPass, IrElementTransformerVoidWithContext() {

    override fun lower(irFile: IrFile) {
        irFile.transformChildrenVoid(this)
    }

    override fun visitBlock(expression: IrBlock): IrExpression {
        if (!shouldLower(expression)) {
            return super.visitBlock(expression)
        }
        val tempVariable = expression.statements[0] as IrVariable
        val enumClass = tempVariable.type.constructor.declarationDescriptor as ClassDescriptor

        val whenExpr = expression.statements[1] as IrWhen
        val getOrdinal = IrCallImpl(tempVariable.startOffset, tempVariable.endOffset, getPropertyByName(enumClass, "ordinal")).apply {
            dispatchReceiver = IrGetValueImpl(tempVariable.startOffset, tempVariable.endOffset, tempVariable.symbol)
        }

        val ordinalDescriptor = IrTemporaryVariableDescriptorImpl(tempVariable.descriptor,
                Name.identifier(tempVariable.name.asString() + "_ordinal"), context.builtIns.intType)
        val ordinalVariable = IrVariableImpl(tempVariable.startOffset, tempVariable.endOffset,
                IrDeclarationOrigin.IR_TEMPORARY_VARIABLE, ordinalDescriptor, getOrdinal)
        expression.statements.add(1, ordinalVariable)

        val getOrdinalVariable = IrGetValueImpl(tempVariable.startOffset, tempVariable.endOffset, ordinalVariable.symbol)

        whenExpr.branches.forEach {
            if (it != whenExpr.branches.last()) {
                val areEqualCall = it.condition as IrCall
                val getCall = areEqualCall.getArguments()[1].second as IrCall
                val index = getCall.getArguments()[1].second as IrConst<Int>
                val ordinal = getOrdinalByIndex(enumClass, index.value)
                // replace condition with trivial comparison of ordinals
                it.condition = IrCallImpl(areEqualCall.startOffset, areEqualCall.endOffset, areEqualByValue).apply {
                    putValueArgument(0, IrConstImpl.int(index.startOffset, index.endOffset, context.builtIns.intType, ordinal))
                    putValueArgument(1, getOrdinalVariable)
                }
            }
        }
        expression.transformChildrenVoid(this)
        return expression
    }

    // Checks that all elements of irBlock satisfy all constrains of this lowering.
    // 1. Block's origin is WHEN
    // 2. Subject of `when` is variable of enum type
    // 3. All branches (except last one) are simple comparisons with enum's entries
    private fun shouldLower(expression: IrBlock): Boolean {
        if (expression.origin != IrStatementOrigin.WHEN) {
            return false
        }
        // when-block should have two children: temporary variable and when itself.
        assert(expression.statements.size == 2)

        val tempVariable = expression.statements[0] as IrVariable

        val enumDescriptor = tempVariable.type.constructor.declarationDescriptor as? ClassDescriptor ?: return false
        if (enumDescriptor.kind != ClassKind.ENUM_CLASS) {
            return false
        }

        val whenExpr = expression.statements[1] as IrWhen
        whenExpr.branches.forEach {
            if (it != whenExpr.branches.last()) {
                val areEqualCall = it.condition as? IrCall ?: return false
                if (areEqualCall.symbol != context.ir.symbols.areEqual) {
                    return false
                }
            } else {
               if (it.condition !is IrConst<*>) {
                   return false
               }
            }
        }
        return true
    }

    private fun getPropertyByName(descriptor: ClassDescriptor, name : String) =
        context.ir.symbols.symbolTable.referenceSimpleFunction(descriptor.unsubstitutedMemberScope
                .getContributedVariables(Name.identifier(name), NoLookupLocation.FROM_BACKEND)
                .single().getter!!)

    private val areEqualByValue = context.ir.symbols.areEqualByValue.first {
        it.owner.valueParameters[0].type == context.builtIns.intType
    }

    // TODO, array index is not same as item's ordinal.
    private fun getOrdinalByIndex(descriptor: ClassDescriptor, index: Int): Int {
        val loweredEnum = context.specialDeclarationsFactory.getLoweredEnum(descriptor)
        val name = loweredEnum.entriesMap.filterValues { it == index }.toList().first().first
        return loweredEnum.ordinals[name]!!
    }
}