package org.jetbrains.kotlin.backend.konan.lower

import org.jetbrains.kotlin.backend.common.FileLoweringPass
import org.jetbrains.kotlin.backend.common.IrElementTransformerVoidWithContext
import org.jetbrains.kotlin.backend.konan.Context
import org.jetbrains.kotlin.backend.konan.irasdescriptors.containingDeclaration
import org.jetbrains.kotlin.descriptors.ClassDescriptor
import org.jetbrains.kotlin.descriptors.ClassKind
import org.jetbrains.kotlin.incremental.components.NoLookupLocation
import org.jetbrains.kotlin.ir.declarations.*
import org.jetbrains.kotlin.ir.declarations.impl.IrVariableImpl
import org.jetbrains.kotlin.ir.descriptors.IrTemporaryVariableDescriptorImpl
import org.jetbrains.kotlin.ir.expressions.*
import org.jetbrains.kotlin.ir.expressions.impl.IrCallImpl
import org.jetbrains.kotlin.ir.expressions.impl.IrGetValueImpl
import org.jetbrains.kotlin.ir.symbols.IrSimpleFunctionSymbol
import org.jetbrains.kotlin.ir.util.getArguments
import org.jetbrains.kotlin.name.Name


internal class EnumOnlyWhenLowering(val context: Context) : FileLoweringPass {
    override fun lower(irFile: IrFile) {
        irFile.transform(EnumOnlyWhenTransformer(context), null)
    }
}

private class EnumOnlyWhenTransformer(val context: Context) : IrElementTransformerVoidWithContext() {
    override fun visitBlock(expression: IrBlock): IrExpression {
        if (expression.origin != IrStatementOrigin.WHEN) {
            return super.visitBlock(expression)
        }
        // when-block should have two children: temporary variable and when itself
        // TODO: this assumption looks fragile
        assert(expression.statements.size == 2)

        val temporaryVariable = expression.statements[0] as IrVariable
        val declarationDescriptor = temporaryVariable.type.constructor.declarationDescriptor
        val enumClassDescriptor = declarationDescriptor as? ClassDescriptor ?: return super.visitBlock(expression)
        if (enumClassDescriptor.kind != ClassKind.ENUM_CLASS) {
            return super.visitBlock(expression)
        }
        val whenExpr = expression.statements[1] as IrWhen
        val getOrdinal = IrCallImpl(temporaryVariable.startOffset, temporaryVariable.endOffset, getPropertyByName(enumClassDescriptor, "ordinal")).apply {
            dispatchReceiver = IrGetValueImpl(temporaryVariable.startOffset, temporaryVariable.endOffset, temporaryVariable.symbol)
        }
        val decl = IrTemporaryVariableDescriptorImpl(temporaryVariable.descriptor, Name.identifier("temp_ordinal"), context.builtIns.intType)
        val ordinalVariable = IrVariableImpl(temporaryVariable.startOffset, temporaryVariable.endOffset, IrDeclarationOrigin.IR_TEMPORARY_VARIABLE, decl, getOrdinal)
        expression.statements.add(1, ordinalVariable)
        val getOrdinalVariable = IrGetValueImpl(temporaryVariable.startOffset, temporaryVariable.endOffset, ordinalVariable.symbol)
        val comparator = context.ir.symbols.areEqualByValue.first {
            it.owner.valueParameters[0].type == context.builtIns.intType
        }
        val loweredEnum = context.specialDeclarationsFactory.getLoweredEnum(enumClassDescriptor)
        whenExpr.branches.forEach {
            // TODO: The last one condition is const
            val areEqualCall = it.condition as? IrCall ?: return super.visitBlock(expression)
            if (areEqualCall.symbol != context.ir.symbols.areEqual) {
                return super.visitBlock(expression)
            }
            val getCall = areEqualCall.getArguments()[1].second as IrCall
            // TODO, array index is not same as item's ordinal.
            val ordinal = getCall.getArguments()[1].second as IrConst<Int>

            it.condition = IrCallImpl(areEqualCall.startOffset, areEqualCall.endOffset, comparator).apply {
                putValueArgument(0, ordinal)
                putValueArgument(1, getOrdinalVariable)
            }
        }
        return expression
    }
    private fun getPropertyByName(descriptor: ClassDescriptor, name : String) : IrSimpleFunctionSymbol =
            context.ir.symbols.symbolTable.referenceSimpleFunction(descriptor.unsubstitutedMemberScope
                    .getContributedVariables(Name.identifier(name), NoLookupLocation.FROM_BACKEND)
                    .single().getter!!)
}