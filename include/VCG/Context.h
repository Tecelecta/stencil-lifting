#pragma once

/**
 * @file Context.h
 * @brief 
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

    /// 
    VCG_API ConstantValue* createConstantValue(Type type, Symbol value, String name = String());

    /// 
    VCG_API InvalidValue* createInvalidValue(Type type, String name = String());

    /// true
    ConstantValue* getTrueValue() const { return trueValue; }

    /// false
    ConstantValue* getFalseValue() const { return falseValue; }

    /// 0
    ConstantValue* getZeroValue() const { return zeroValue; }

private:
    ConstantValue* trueValue = nullptr;
    MarkedObjectGuard trueValueGuard;
    ConstantValue* falseValue = nullptr;
    MarkedObjectGuard falseValueGuard;
    ConstantValue* zeroValue = nullptr;
    MarkedObjectGuard zeroValueGuard;
};
