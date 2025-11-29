package io.github.luminion.sqlbooster.builder;

import io.github.luminion.sqlbooster.core.TableMetaRegistry;
import io.github.luminion.sqlbooster.function.GetterReference;
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

    /**
     * 添加一组 OR 连接的条件。
     */
    public S or(Consumer<S> consumer) {
        S newInstance = newInstance();
        newInstance.sqlContext.setConnector(SqlKeyword.OR.getSymbol());
        consumer.accept(newInstance);
        this.sqlContext.merge(newInstance.sqlContext);
        return (S) this;
    }

    public <R> S eq(GetterReference<T, R> getter, R value) {
        if (value == null) {
            return (S) this;
        }
        this.sqlContext.getConditions().add(new Condition(TableMetaRegistry.getGetterPropertyName(getter), SqlKeyword.EQ.getSymbol(), value));
        return (S) this;
    }

    public <R> S ne(GetterReference<T, R> getter, R value) {
        if (value == null) {
            return (S) this;
        }
        this.sqlContext.getConditions().add(new Condition(TableMetaRegistry.getGetterPropertyName(getter), SqlKeyword.NE.getSymbol(), value));
        return (S) this;
    }

    public <R> S gt(GetterReference<T, R> getter, R value) {
        if (value == null) {
            return (S) this;
        }
        this.sqlContext.getConditions().add(new Condition(TableMetaRegistry.getGetterPropertyName(getter), SqlKeyword.GT.getSymbol(), value));
        return (S) this;
    }

    public <R> S ge(GetterReference<T, R> getter, R value) {
        if (value == null) {
            return (S) this;
        }
        this.sqlContext.getConditions().add(new Condition(TableMetaRegistry.getGetterPropertyName(getter), SqlKeyword.GTE.getSymbol(), value));
        return (S) this;
    }

    public <R> S lt(GetterReference<T, R> getter, R value) {
        if (value == null) {
            return (S) this;
        }
        this.sqlContext.getConditions().add(new Condition(TableMetaRegistry.getGetterPropertyName(getter), SqlKeyword.LT.getSymbol(), value));
        return (S) this;
    }

    public <R> S le(GetterReference<T, R> getter, R value) {
        if (value == null) {
            return (S) this;
        }
        this.sqlContext.getConditions().add(new Condition(TableMetaRegistry.getGetterPropertyName(getter), SqlKeyword.LTE.getSymbol(), value));
        return (S) this;
    }

    public <R> S like(GetterReference<T, R> getter, R value) {
        if (value == null) {
            return (S) this;
        }
        this.sqlContext.getConditions().add(new Condition(TableMetaRegistry.getGetterPropertyName(getter), SqlKeyword.LIKE.getSymbol(), value));
        return (S) this;
    }

    public <R> S notLike(GetterReference<T, R> getter, R value) {
        if (value == null) {
            return (S) this;
        }
        this.sqlContext.getConditions().add(new Condition(TableMetaRegistry.getGetterPropertyName(getter), SqlKeyword.NOT_LIKE.getSymbol(), value));
        return (S) this;
    }

    public <R> S in(GetterReference<T, R> getter, Collection<? extends R> value) {
        if (value == null) {
            return (S) this;
        }
        this.sqlContext.getConditions().add(new Condition(TableMetaRegistry.getGetterPropertyName(getter), SqlKeyword.IN.getSymbol(), value));
        return (S) this;
    }

    public <R> S notIn(GetterReference<T, R> getter, Collection<? extends R> value) {
        if (value == null) {
            return (S) this;
        }
        this.sqlContext.getConditions().add(new Condition(TableMetaRegistry.getGetterPropertyName(getter), SqlKeyword.NOT_IN.getSymbol(), value));
        return (S) this;
    }

    public S isNull(GetterReference<T, ?> getter) {
        this.sqlContext.getConditions().add(new Condition(TableMetaRegistry.getGetterPropertyName(getter), SqlKeyword.IS_NULL.getSymbol(), true));
        return (S) this;
    }

    public S isNotNull(GetterReference<T, ?> getter) {
        this.sqlContext.getConditions().add(new Condition(TableMetaRegistry.getGetterPropertyName(getter), SqlKeyword.IS_NOT_NULL.getSymbol(), true));
        return (S) this;
    }

    public S orderByAsc(GetterReference<T, ?> getter) {
        this.sqlContext.getSorts().add(new Sort(TableMetaRegistry.getGetterPropertyName(getter), true));
        return (S) this;
    }

    public S orderByDesc(GetterReference<T, ?> getter) {
        this.sqlContext.getSorts().add(new Sort(TableMetaRegistry.getGetterPropertyName(getter), false));
        return (S) this;
    }

    /**
     * [位运算] 包含任意位 (Any).
     * <p>相当于 SQL: {@code (column & value) > 0}</p>
     * 只要 value 中任意一个为 1 的位，在数据库字段中也为 1，即符合条件。
     */
    public <R> S bitAny(GetterReference<T, R> getter, R value) {
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
    public <R> S bitAll(GetterReference<T, R> getter, R value) {
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
    public <R> S bitNone(GetterReference<T, R> getter, R value) {
        if (value == null) {
            return (S) this;
        }
        this.sqlContext.getConditions().add(new Condition(TableMetaRegistry.getGetterPropertyName(getter), SqlKeyword.BIT_NONE.getSymbol(), value));
        return (S) this;
    }

}
