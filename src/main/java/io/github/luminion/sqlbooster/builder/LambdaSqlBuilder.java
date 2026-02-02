package io.github.luminion.sqlbooster.builder;

import io.github.luminion.sqlbooster.core.TableMetaRegistry;
import io.github.luminion.sqlbooster.function.SFunc;
import io.github.luminion.sqlbooster.enums.SqlKeyword;
import io.github.luminion.sqlbooster.model.query.Condition;
import io.github.luminion.sqlbooster.model.query.Sort;

import java.util.Collection;
import java.util.function.Consumer;

/**
 * 使用 Lambda 表达式构建 SQL 的 Builder。
 * <p>
 * 提供一套流畅的 API 用于构建 SQL 查询条件。
 *
 * @param <T> 实体类型
 * @param <S> 用于链式调用的返回类型
 */
@SuppressWarnings({"unchecked", "unused"})
public abstract class LambdaSqlBuilder<T, S extends LambdaSqlBuilder<T, S>> extends AbstractSqlBuilder<T, S> {

    public LambdaSqlBuilder(Class<T> entityClass) {
        super(entityClass);
    }

    public S orderByAsc(SFunc<T, ?> getter) {
        this.sqlContext.getSorts().add(new Sort(TableMetaRegistry.getGetterPropertyName(getter), true));
        return (S) this;
    }

    public S orderByDesc(SFunc<T, ?> getter) {
        this.sqlContext.getSorts().add(new Sort(TableMetaRegistry.getGetterPropertyName(getter), false));
        return (S) this;
    }
    
    /**
     * 添加一组 OR 连接的条件。
     */
    public S or(Consumer<S> consumer) {
        S newInstance = newInstance();
        consumer.accept(newInstance);
        newInstance.sqlContext.setAnd(false);
        this.sqlContext.merge(newInstance.sqlContext);
        return (S) this;
    }

    public <R> S eq(SFunc<T, R> getter, R value) {
        if (value == null) {
            return (S) this;
        }
        this.sqlContext.getConditions().add(new Condition(TableMetaRegistry.getGetterPropertyName(getter), SqlKeyword.EQ.getSymbol(), value));
        return (S) this;
    }

    public <R> S ne(SFunc<T, R> getter, R value) {
        if (value == null) {
            return (S) this;
        }
        this.sqlContext.getConditions().add(new Condition(TableMetaRegistry.getGetterPropertyName(getter), SqlKeyword.NE.getSymbol(), value));
        return (S) this;
    }

    public <R> S gt(SFunc<T, R> getter, R value) {
        if (value == null) {
            return (S) this;
        }
        this.sqlContext.getConditions().add(new Condition(TableMetaRegistry.getGetterPropertyName(getter), SqlKeyword.GT.getSymbol(), value));
        return (S) this;
    }

    public <R> S gte(SFunc<T, R> getter, R value) {
        if (value == null) {
            return (S) this;
        }
        this.sqlContext.getConditions().add(new Condition(TableMetaRegistry.getGetterPropertyName(getter), SqlKeyword.GTE.getSymbol(), value));
        return (S) this;
    }

    public <R> S lt(SFunc<T, R> getter, R value) {
        if (value == null) {
            return (S) this;
        }
        this.sqlContext.getConditions().add(new Condition(TableMetaRegistry.getGetterPropertyName(getter), SqlKeyword.LT.getSymbol(), value));
        return (S) this;
    }

    public <R> S lte(SFunc<T, R> getter, R value) {
        if (value == null) {
            return (S) this;
        }
        this.sqlContext.getConditions().add(new Condition(TableMetaRegistry.getGetterPropertyName(getter), SqlKeyword.LTE.getSymbol(), value));
        return (S) this;
    }

    public <R> S like(SFunc<T, R> getter, R value) {
        if (value == null) {
            return (S) this;
        }
        this.sqlContext.getConditions().add(new Condition(TableMetaRegistry.getGetterPropertyName(getter), SqlKeyword.LIKE.getSymbol(), value));
        return (S) this;
    }

    public <R> S notLike(SFunc<T, R> getter, R value) {
        if (value == null) {
            return (S) this;
        }
        this.sqlContext.getConditions().add(new Condition(TableMetaRegistry.getGetterPropertyName(getter), SqlKeyword.NOT_LIKE.getSymbol(), value));
        return (S) this;
    }

    public <R> S in(SFunc<T, R> getter, Collection<? extends R> value) {
        if (value == null) {
            return (S) this;
        }
        this.sqlContext.getConditions().add(new Condition(TableMetaRegistry.getGetterPropertyName(getter), SqlKeyword.IN.getSymbol(), value));
        return (S) this;
    }

    public <R> S notIn(SFunc<T, R> getter, Collection<? extends R> value) {
        if (value == null) {
            return (S) this;
        }
        this.sqlContext.getConditions().add(new Condition(TableMetaRegistry.getGetterPropertyName(getter), SqlKeyword.NOT_IN.getSymbol(), value));
        return (S) this;
    }

    public S isNull(SFunc<T, ?> getter) {
        this.sqlContext.getConditions().add(new Condition(TableMetaRegistry.getGetterPropertyName(getter), SqlKeyword.IS_NULL.getSymbol(), true));
        return (S) this;
    }

    public S isNotNull(SFunc<T, ?> getter) {
        this.sqlContext.getConditions().add(new Condition(TableMetaRegistry.getGetterPropertyName(getter), SqlKeyword.IS_NOT_NULL.getSymbol(), true));
        return (S) this;
    }

    /**
     * [位运算] 包含任意位 (Any).
     * <p>相当于 SQL: {@code (column & value) > 0}</p>
     * 只要 value 中任意一个为 1 的位，在数据库字段中也为 1，即符合条件。
     */
    public <R> S bitAny(SFunc<T, R> getter, R value) {
        if (value == null) {
            return (S) this;
        }
        this.sqlContext.getConditions().add(new Condition(TableMetaRegistry.getGetterPropertyName(getter), SqlKeyword.BIT_ANY.getSymbol(), value));
        return (S) this;
    }

    /**
     * [位运算] 包含所有位 (All).
     * <p>相当于 SQL: {@code (column & value) = value}</p>
     * value 中所有为 1 的位，在数据库字段中必须都为 1，才符合条件。
     */
    public <R> S bitAll(SFunc<T, R> getter, R value) {
        if (value == null) {
            return (S) this;
        }
        this.sqlContext.getConditions().add(new Condition(TableMetaRegistry.getGetterPropertyName(getter), SqlKeyword.BIT_ALL.getSymbol(), value));
        return (S) this;
    }

    /**
     * [位运算] 不包含任何位 (None).
     * <p>相当于 SQL: {@code (column & value) = 0}</p>
     * value 中所有为 1 的位，在数据库字段中必须都为 0，才符合条件。
     */
    public <R> S bitNone(SFunc<T, R> getter, R value) {
        if (value == null) {
            return (S) this;
        }
        this.sqlContext.getConditions().add(new Condition(TableMetaRegistry.getGetterPropertyName(getter), SqlKeyword.BIT_NONE.getSymbol(), value));
        return (S) this;
    }

}
