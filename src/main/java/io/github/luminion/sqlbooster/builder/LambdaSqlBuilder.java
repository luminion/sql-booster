package io.github.luminion.sqlbooster.builder;

import io.github.luminion.sqlbooster.enums.SqlKeyword;
import io.github.luminion.sqlbooster.function.SFunc;
import io.github.luminion.sqlbooster.metadata.TableMetaRegistry;
import io.github.luminion.sqlbooster.model.query.Condition;
import io.github.luminion.sqlbooster.model.query.Sort;

import java.util.Collection;
import java.util.function.Consumer;

/**
 * 使用 Lambda 表达式构建 SQL 的基类。
 *
 * @param <T> 实体类型
 * @param <S> 用于链式调用的返回类型
 */
@SuppressWarnings({"unchecked", "unused"})
public abstract class LambdaSqlBuilder<T, S extends LambdaSqlBuilder<T, S>> extends AbstractSqlBuilder<T, S> {

    protected LambdaSqlBuilder(Class<T> entityClass) {
        super(entityClass);
    }

    public S orderByAsc(SFunc<T, ?> getter) {
        return addSort(new Sort(TableMetaRegistry.getGetterPropertyName(getter), true));
    }

    public S orderByDesc(SFunc<T, ?> getter) {
        return addSort(new Sort(TableMetaRegistry.getGetterPropertyName(getter), false));
    }

    /**
     * 添加一组 OR 连接的条件。
     */
    public S or(Consumer<S> consumer) {
        S nested = newInstance();
        consumer.accept(nested);
        nested.sqlContext.setAnd(false);
        this.sqlContext.merge(nested.sqlContext);
        return (S) this;
    }

    public <R> S eq(SFunc<T, R> getter, R value) {
        if (value == null) {
            return (S) this;
        }
        return addCondition(new Condition(TableMetaRegistry.getGetterPropertyName(getter),
                SqlKeyword.EQ.getSymbol(), value));
    }

    public <R> S ne(SFunc<T, R> getter, R value) {
        if (value == null) {
            return (S) this;
        }
        return addCondition(new Condition(TableMetaRegistry.getGetterPropertyName(getter),
                SqlKeyword.NE.getSymbol(), value));
    }

    public <R> S gt(SFunc<T, R> getter, R value) {
        if (value == null) {
            return (S) this;
        }
        return addCondition(new Condition(TableMetaRegistry.getGetterPropertyName(getter),
                SqlKeyword.GT.getSymbol(), value));
    }

    public <R> S gte(SFunc<T, R> getter, R value) {
        if (value == null) {
            return (S) this;
        }
        return addCondition(new Condition(TableMetaRegistry.getGetterPropertyName(getter),
                SqlKeyword.GTE.getSymbol(), value));
    }

    public <R> S ge(SFunc<T, R> getter, R value) {
        return gte(getter, value);
    }

    public <R> S lt(SFunc<T, R> getter, R value) {
        if (value == null) {
            return (S) this;
        }
        return addCondition(new Condition(TableMetaRegistry.getGetterPropertyName(getter),
                SqlKeyword.LT.getSymbol(), value));
    }

    public <R> S lte(SFunc<T, R> getter, R value) {
        if (value == null) {
            return (S) this;
        }
        return addCondition(new Condition(TableMetaRegistry.getGetterPropertyName(getter),
                SqlKeyword.LTE.getSymbol(), value));
    }

    public <R> S le(SFunc<T, R> getter, R value) {
        return lte(getter, value);
    }

    public <R> S like(SFunc<T, R> getter, R value) {
        if (value == null) {
            return (S) this;
        }
        return addCondition(new Condition(TableMetaRegistry.getGetterPropertyName(getter),
                SqlKeyword.LIKE.getSymbol(), value));
    }

    public <R> S notLike(SFunc<T, R> getter, R value) {
        if (value == null) {
            return (S) this;
        }
        return addCondition(new Condition(TableMetaRegistry.getGetterPropertyName(getter),
                SqlKeyword.NOT_LIKE.getSymbol(), value));
    }

    public <R> S in(SFunc<T, R> getter, Collection<? extends R> value) {
        if (value == null) {
            return (S) this;
        }
        return addCondition(new Condition(TableMetaRegistry.getGetterPropertyName(getter),
                SqlKeyword.IN.getSymbol(), value));
    }

    public <R> S notIn(SFunc<T, R> getter, Collection<? extends R> value) {
        if (value == null) {
            return (S) this;
        }
        return addCondition(new Condition(TableMetaRegistry.getGetterPropertyName(getter),
                SqlKeyword.NOT_IN.getSymbol(), value));
    }

    public S isNull(SFunc<T, ?> getter) {
        return addCondition(new Condition(TableMetaRegistry.getGetterPropertyName(getter),
                SqlKeyword.IS_NULL.getSymbol(), true));
    }

    public S isNotNull(SFunc<T, ?> getter) {
        return addCondition(new Condition(TableMetaRegistry.getGetterPropertyName(getter),
                SqlKeyword.IS_NOT_NULL.getSymbol(), true));
    }

    public <R> S hasAnyBits(SFunc<T, R> getter, R value) {
        if (value == null) {
            return (S) this;
        }
        return addCondition(new Condition(TableMetaRegistry.getGetterPropertyName(getter),
                SqlKeyword.HAS_ANY_BITS.getSymbol(), value));
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

    @Deprecated
    public <R> S bitNone(SFunc<T, R> getter, R value) {
        return hasNoBits(getter, value);
    }
}