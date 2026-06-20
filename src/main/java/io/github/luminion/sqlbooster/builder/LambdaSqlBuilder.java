package io.github.luminion.sqlbooster.builder;

import io.github.luminion.sqlbooster.enums.SqlKeyword;
import io.github.luminion.sqlbooster.function.SFunc;
import io.github.luminion.sqlbooster.metadata.TableMetaRegistry;
import io.github.luminion.sqlbooster.model.Condition;
import io.github.luminion.sqlbooster.model.Sort;

import java.util.Collection;
import java.util.function.Consumer;

@SuppressWarnings({"unchecked", "unused"})
public abstract class LambdaSqlBuilder<T, S extends LambdaSqlBuilder<T, S>> extends AbstractSqlBuilder<T, S> {

    protected LambdaSqlBuilder(Class<T> entityClass) {
        super(entityClass);
    }

    public S orderByAsc(SFunc<T, ?> getter) {
        return addSort(new Sort(TableMetaRegistry.getGetterPropertyName(getter), true));
    }

    public S orderByAsc(boolean condition, SFunc<T, ?> getter) {
        return condition ? orderByAsc(getter) : (S) this;
    }

    public S orderByDesc(SFunc<T, ?> getter) {
        return addSort(new Sort(TableMetaRegistry.getGetterPropertyName(getter), false));
    }

    public S orderByDesc(boolean condition, SFunc<T, ?> getter) {
        return condition ? orderByDesc(getter) : (S) this;
    }

    public S or(Consumer<S> consumer) {
        S nested = newInstance();
        consumer.accept(nested);
        // `or(...)` 不是把条件平铺到当前节点，而是单独挂一个 OR 节点，
        // 后面生成 SQL 时才能保住分组语义。
        nested.sqlContext.setAnd(false);
        this.sqlContext.merge(nested.sqlContext);
        return (S) this;
    }

    public S or(boolean condition, Consumer<S> consumer) {
        return condition ? or(consumer) : (S) this;
    }

    public <R> S eq(SFunc<T, R> getter, R value) {
        if (value == null) {
            return (S) this;
        }
        return addCondition(new Condition(TableMetaRegistry.getGetterPropertyName(getter),
                SqlKeyword.EQ.getSymbol(), value));
    }

    public <R> S eq(boolean condition, SFunc<T, R> getter, R value) {
        return condition ? eq(getter, value) : (S) this;
    }

    public <R> S ne(SFunc<T, R> getter, R value) {
        if (value == null) {
            return (S) this;
        }
        return addCondition(new Condition(TableMetaRegistry.getGetterPropertyName(getter),
                SqlKeyword.NE.getSymbol(), value));
    }

    public <R> S ne(boolean condition, SFunc<T, R> getter, R value) {
        return condition ? ne(getter, value) : (S) this;
    }

    public <R> S gt(SFunc<T, R> getter, R value) {
        if (value == null) {
            return (S) this;
        }
        return addCondition(new Condition(TableMetaRegistry.getGetterPropertyName(getter),
                SqlKeyword.GT.getSymbol(), value));
    }

    public <R> S gt(boolean condition, SFunc<T, R> getter, R value) {
        return condition ? gt(getter, value) : (S) this;
    }

    public <R> S gte(SFunc<T, R> getter, R value) {
        if (value == null) {
            return (S) this;
        }
        return addCondition(new Condition(TableMetaRegistry.getGetterPropertyName(getter),
                SqlKeyword.GTE.getSymbol(), value));
    }

    public <R> S gte(boolean condition, SFunc<T, R> getter, R value) {
        return condition ? gte(getter, value) : (S) this;
    }

    public <R> S ge(SFunc<T, R> getter, R value) {
        return gte(getter, value);
    }

    public <R> S ge(boolean condition, SFunc<T, R> getter, R value) {
        return condition ? gte(getter, value) : (S) this;
    }

    public <R> S lt(SFunc<T, R> getter, R value) {
        if (value == null) {
            return (S) this;
        }
        return addCondition(new Condition(TableMetaRegistry.getGetterPropertyName(getter),
                SqlKeyword.LT.getSymbol(), value));
    }

    public <R> S lt(boolean condition, SFunc<T, R> getter, R value) {
        return condition ? lt(getter, value) : (S) this;
    }

    public <R> S lte(SFunc<T, R> getter, R value) {
        if (value == null) {
            return (S) this;
        }
        return addCondition(new Condition(TableMetaRegistry.getGetterPropertyName(getter),
                SqlKeyword.LTE.getSymbol(), value));
    }

    public <R> S lte(boolean condition, SFunc<T, R> getter, R value) {
        return condition ? lte(getter, value) : (S) this;
    }

    public <R> S le(SFunc<T, R> getter, R value) {
        return lte(getter, value);
    }

    public <R> S le(boolean condition, SFunc<T, R> getter, R value) {
        return condition ? lte(getter, value) : (S) this;
    }

    public <R> S like(SFunc<T, R> getter, R value) {
        if (value == null) {
            return (S) this;
        }
        return addCondition(new Condition(TableMetaRegistry.getGetterPropertyName(getter),
                SqlKeyword.LIKE.getSymbol(), value));
    }

    public <R> S like(boolean condition, SFunc<T, R> getter, R value) {
        return condition ? like(getter, value) : (S) this;
    }

    public <R> S notLike(SFunc<T, R> getter, R value) {
        if (value == null) {
            return (S) this;
        }
        return addCondition(new Condition(TableMetaRegistry.getGetterPropertyName(getter),
                SqlKeyword.NOT_LIKE.getSymbol(), value));
    }

    public <R> S notLike(boolean condition, SFunc<T, R> getter, R value) {
        return condition ? notLike(getter, value) : (S) this;
    }

    public <R> S in(SFunc<T, R> getter, Collection<? extends R> value) {
        if (value == null) {
            return (S) this;
        }
        return addCondition(new Condition(TableMetaRegistry.getGetterPropertyName(getter),
                SqlKeyword.IN.getSymbol(), value));
    }

    public <R> S in(boolean condition, SFunc<T, R> getter, Collection<? extends R> value) {
        return condition ? in(getter, value) : (S) this;
    }

    public <R> S notIn(SFunc<T, R> getter, Collection<? extends R> value) {
        if (value == null) {
            return (S) this;
        }
        return addCondition(new Condition(TableMetaRegistry.getGetterPropertyName(getter),
                SqlKeyword.NOT_IN.getSymbol(), value));
    }

    public <R> S notIn(boolean condition, SFunc<T, R> getter, Collection<? extends R> value) {
        return condition ? notIn(getter, value) : (S) this;
    }

    public S isNull(SFunc<T, ?> getter) {
        return addCondition(new Condition(TableMetaRegistry.getGetterPropertyName(getter),
                SqlKeyword.IS_NULL.getSymbol(), true));
    }

    public S isNull(boolean condition, SFunc<T, ?> getter) {
        return condition ? isNull(getter) : (S) this;
    }

    public S isNotNull(SFunc<T, ?> getter) {
        return addCondition(new Condition(TableMetaRegistry.getGetterPropertyName(getter),
                SqlKeyword.IS_NOT_NULL.getSymbol(), true));
    }

    public S isNotNull(boolean condition, SFunc<T, ?> getter) {
        return condition ? isNotNull(getter) : (S) this;
    }

    public <R> S hasAnyBits(SFunc<T, R> getter, R value) {
        if (value == null) {
            return (S) this;
        }
        return addCondition(new Condition(TableMetaRegistry.getGetterPropertyName(getter),
                SqlKeyword.HAS_ANY_BITS.getSymbol(), value));
    }

    public <R> S hasAnyBits(boolean condition, SFunc<T, R> getter, R value) {
        return condition ? hasAnyBits(getter, value) : (S) this;
    }

    @Deprecated
    public <R> S bitAny(SFunc<T, R> getter, R value) {
        return hasAnyBits(getter, value);
    }

    public <R> S hasAllBits(SFunc<T, R> getter, R value) {
        if (value == null) {
            return (S) this;
        }
        return addCondition(new Condition(TableMetaRegistry.getGetterPropertyName(getter),
                SqlKeyword.HAS_ALL_BITS.getSymbol(), value));
    }

    public <R> S hasAllBits(boolean condition, SFunc<T, R> getter, R value) {
        return condition ? hasAllBits(getter, value) : (S) this;
    }

    @Deprecated
    public <R> S bitAll(SFunc<T, R> getter, R value) {
        return hasAllBits(getter, value);
    }

    public <R> S hasNoBits(SFunc<T, R> getter, R value) {
        if (value == null) {
            return (S) this;
        }
        return addCondition(new Condition(TableMetaRegistry.getGetterPropertyName(getter),
                SqlKeyword.HAS_NO_BITS.getSymbol(), value));
    }

    public <R> S hasNoBits(boolean condition, SFunc<T, R> getter, R value) {
        return condition ? hasNoBits(getter, value) : (S) this;
    }

    @Deprecated
    public <R> S bitNone(SFunc<T, R> getter, R value) {
        return hasNoBits(getter, value);
    }
}
