package org.jetbrains.kotlin.backend.konan.lower

import org.jetbrains.kotlin.backend.common.FileLoweringPass
import org.jetbrains.kotlin.backend.common.IrElementTransformerVoidWithContext
import org.jetbrains.kotlin.backend.konan.Context
import org.jetbrains.kotlin.backend.konan.descriptors.contributedMethods
import org.jetbrains.kotlin.backend.konan.irasdescriptors.containingDeclaration
import org.jetbrains.kotlin.descriptors.ClassDescriptor
import org.jetbrains.kotlin.descriptors.ClassKind
import org.jetbrains.kotlin.descriptors.PropertyDescriptor
import org.jetbrains.kotlin.incremental.components.NoLookupLocation
import org.jetbrains.kotlin.ir.declarations.*
import org.jetbrains.kotlin.ir.declarations.impl.IrVariableImpl
import org.jetbrains.kotlin.ir.descriptors.IrTemporaryVariableDescriptor
import org.jetbrains.kotlin.ir.descriptors.IrTemporaryVariableDescriptorImpl
import org.jetbrains.kotlin.ir.expressions.*
import org.jetbrains.kotlin.ir.expressions.impl.IrCallImpl
import org.jetbrains.kotlin.ir.expressions.impl.IrGetValueImpl
import org.jetbrains.kotlin.ir.symbols.IrSimpleFunctionSymbol
import org.jetbrains.kotlin.ir.util.endOffset
import org.jetbrains.kotlin.ir.util.getArguments
import org.jetbrains.kotlin.ir.util.startOffset
import org.jetbrains.kotlin.name.Name
import org.jetbrains.kotlin.types.TypeUtils


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
        context.log { "Found when statement" }
        // when-block should have two children: temporary variable and when itself
        // TODO: this assumption looks fragile
        assert(expression.statements.size == 2)

        val temporaryVariable = expression.statements[0] as IrVariable
        val declarationDescriptor = temporaryVariable.type.constructor.declarationDescriptor
        val variableClass = if (declarationDescriptor is ClassDescriptor) {
            declarationDescriptor
        } else {
            context.log { "$declarationDescriptor is not ClassDescriptor" }
            return super.visitBlock(expression)
        }
        if (variableClass.kind != ClassKind.ENUM_CLASS) {
            context.log { "${variableClass.kind} is not enum_entry" }
            return super.visitBlock(expression)
        }
        val whenExpr = expression.statements[1] as IrWhen
        val getOrdinal = IrCallImpl(temporaryVariable.startOffset, temporaryVariable.endOffset, getPropertyByName(variableClass, "ordinal")).apply {
            dispatchReceiver = IrGetValueImpl(temporaryVariable.startOffset, temporaryVariable.endOffset, temporaryVariable.symbol)
        }
        val decl = IrTemporaryVariableDescriptorImpl(temporaryVariable.descriptor, Name.identifier("lolkek"), context.builtIns.intType)
        val ordinalVariable = IrVariableImpl(temporaryVariable.startOffset, temporaryVariable.endOffset, IrDeclarationOrigin.IR_TEMPORARY_VARIABLE, decl, getOrdinal)
        expression.statements.add(1, ordinalVariable)
        whenExpr.branches.forEach {
            if (it.condition !is IrCall) {
                return super.visitBlock(expression)
            }
            val areEqualCall = it.condition as IrCall
            if (areEqualCall.symbol != context.ir.symbols.areEqual) {
                return super.visitBlock(expression)
            }
            val getCall = areEqualCall.getArguments()[1].second as IrCall
            val ordinal = getCall.getArguments()[1].second as IrConst<Int>
            val comparator = context.ir.symbols.areEqualByValue.first {
                it.owner.valueParameters[0].type == ordinal.type
            }
            it.condition = IrCallImpl(areEqualCall.startOffset, areEqualCall.endOffset, comparator).apply {
                putValueArgument(0, ordinal)
                putValueArgument(1, IrGetValueImpl(temporaryVariable.startOffset, temporaryVariable.endOffset, ordinalVariable.symbol))
            }
        }
        return expression
    }
    private fun getPropertyByName(descriptor: ClassDescriptor, name : String) : IrSimpleFunctionSymbol =
            context.ir.symbols.symbolTable.referenceSimpleFunction(descriptor.unsubstitutedMemberScope
                    .getContributedVariables(Name.identifier(name), NoLookupLocation.FROM_BACKEND)
                    .single().getter!!)
}



private fun IrEnumEntry.getOrdinal(context: Context): Int {
    val enumClassDescriptor = this.containingDeclaration as ClassDescriptor
    val loweredEnum = context.specialDeclarationsFactory.getLoweredEnum(enumClassDescriptor)
    return loweredEnum.entriesMap[this.name]!!
}