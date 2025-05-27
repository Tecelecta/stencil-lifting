#pragma once

/**
 * @file Context.h
 * @brief 定义符号计算和内存管理的上下文
 */
#define VCG_EXPORTS

#include "GarbageCollector.h"
#include "Section.h"

class Context final : public GarbageCollector,
    public String::Factory, public Enum::Factory, public BitVector::Factory, public Integer::Factory, public Decimal::Factory,
    public Type::Factory, public Operation::Factory
{
public:
    VCG_API Context();
    VCG_API ~Context();
    DISABLE_COPY_MOVE(Context)

    /// 获取常量字面量
    VCG_API ConstantValue* createConstantValue(Type type, Symbol value, String name = String());

    /// 获取无效值
    VCG_API InvalidValue* createInvalidValue(Type type, String name = String());

    /// 获取值为true的常量值
    ConstantValue* getTrueValue() const { return trueValue; }

    /// 获取值为false的常量值
    ConstantValue* getFalseValue() const { return falseValue; }

    /// 获取值为整型0的常量值
    ConstantValue* getZeroValue() const { return zeroValue; }

private:
    ConstantValue* trueValue = nullptr;
    MarkedObjectGuard trueValueGuard;
    ConstantValue* falseValue = nullptr;
    MarkedObjectGuard falseValueGuard;
    ConstantValue* zeroValue = nullptr;
    MarkedObjectGuard zeroValueGuard;
};
