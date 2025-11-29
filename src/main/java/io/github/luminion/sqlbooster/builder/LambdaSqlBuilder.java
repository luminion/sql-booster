package io.github.luminion.sqlbooster.builder;

import io.github.luminion.sqlbooster.core.TableMetaRegistry;
import io.github.luminion.sqlbooster.function.GetterReference;
import io.github.luminion.sqlbooster.enums.SqlKeyword;
import io.github.luminion.sqlbooster.model.query.Condition;
import io.github.luminion.sqlbooster.model.query.Sort;

import java.util.Collection;
import java.util.function.Consumer;

/**
 * Lambda 风格的 SQL 构建助手接口.
 * <p>
 * 提供基于 Lambda 表达式的 SQL 条件构建方法, 支持链式调用.
 *
 * @param <T> 实体类型
 * @param <S> 返回类型 (用于支持链式调用)
 * @author luminion
 * @since 1.0.0
 */
@SuppressWarnings({"unchecked", "unused"})
public abstract class LambdaSqlBuilder<T, S extends LambdaSqlBuilder<T, S>> extends AbstractSqlBuilder<T, S> {

    public LambdaSqlBuilder(Class<T> entityClass) {
        super(entityClass);
    }

    /**
     * 添加一组 OR 连接的条件.
     *
     * @param consumer 用于构建 OR 条件的 Consumer
     * @return 当前实例
     * @since 1.0.0
     */
    public S or(Consumer<S> consumer) {
        S newInstance = newInstance();
        newInstance.sqlContext.setConnector(SqlKeyword.OR.getKeyword());
        consumer.accept(newInstance);
        this.sqlContext.merge(newInstance.sqlContext);
        return (S) this;
    }


    /**
     * 添加等于 (EQ) 条件.
     *
     * @param getter 字段的 getter 方法引用
     * @param value  条件值
     * @param <R>    字段类型
     * @return 当前实例
     * @since 1.0.0
     */
    public <R> S eq(GetterReference<T, R> getter, R value) {
        if (value == null) {
            return (S) this;
        }
        this.sqlContext.getConditions().add(new Condition(TableMetaRegistry.getGetterPropertyName(getter), SqlKeyword.EQ.getKeyword(), value));
        return (S) this;
    }

    /**
     * 添加不等于 (NE) 条件.
     *
     * @param getter 字段的 getter 方法引用
     * @param value  条件值
     * @param <R>    字段类型
     * @return 当前实例
     * @since 1.0.0
     */
    public <R> S ne(GetterReference<T, R> getter, R value) {
        if (value == null) {
            return (S) this;
        }
        this.sqlContext.getConditions().add(new Condition(TableMetaRegistry.getGetterPropertyName(getter), SqlKeyword.NE.getKeyword(), value));
        return (S) this;
    }

    /**
     * 添加大于 (GT) 条件.
     *
     * @param getter 字段的 getter 方法引用
     * @param value  条件值
     * @param <R>    字段类型
     * @return 当前实例
     * @since 1.0.0
     */
    public <R> S gt(GetterReference<T, R> getter, R value) {
        if (value == null) {
            return (S) this;
        }
        this.sqlContext.getConditions().add(new Condition(TableMetaRegistry.getGetterPropertyName(getter), SqlKeyword.GT.getKeyword(), value));
        return (S) this;
    }

    /**
     * 添加大于等于 (GE) 条件.
     *
     * @param getter 字段的 getter 方法引用
     * @param value  条件值
     * @param <R>    字段类型
     * @return 当前实例
     * @since 1.0.0
     */
    public <R> S ge(GetterReference<T, R> getter, R value) {
        if (value == null) {
            return (S) this;
        }
        this.sqlContext.getConditions().add(new Condition(TableMetaRegistry.getGetterPropertyName(getter), SqlKeyword.GTE.getKeyword(), value));
        return (S) this;
    }

    /**
     * 添加小于 (LT) 条件.
     *
     * @param getter 字段的 getter 方法引用
     * @param value  条件值
     * @param <R>    字段类型
     * @return 当前实例
     * @since 1.0.0
     */
    public <R> S lt(GetterReference<T, R> getter, R value) {
        if (value == null) {
            return (S) this;
        }
        this.sqlContext.getConditions().add(new Condition(TableMetaRegistry.getGetterPropertyName(getter), SqlKeyword.LT.getKeyword(), value));
        return (S) this;
    }

    /**
     * 添加小于等于 (LE) 条件.
     *
     * @param getter 字段的 getter 方法引用
     * @param value  条件值
     * @param <R>    字段类型
     * @return 当前实例
     * @since 1.0.0
     */
    public <R> S le(GetterReference<T, R> getter, R value) {
        if (value == null) {
            return (S) this;
        }
        this.sqlContext.getConditions().add(new Condition(TableMetaRegistry.getGetterPropertyName(getter), SqlKeyword.LTE.getKeyword(), value));
        return (S) this;
    }

    /**
     * 添加 LIKE 条件.
     *
     * @param getter 字段的 getter 方法引用
     * @param value  条件值
     * @param <R>    字段类型
     * @return 当前实例
     * @since 1.0.0
     */
    public <R> S like(GetterReference<T, R> getter, R value) {
        if (value == null) {
            return (S) this;
        }
        this.sqlContext.getConditions().add(new Condition(TableMetaRegistry.getGetterPropertyName(getter), SqlKeyword.LIKE.getKeyword(), value));
        return (S) this;
    }

    /**
     * 添加 NOT LIKE 条件.
     *
     * @param getter 字段的 getter 方法引用
     * @param value  条件值
     * @param <R>    字段类型
     * @return 当前实例
     * @since 1.0.0
     */
    public <R> S notLike(GetterReference<T, R> getter, R value) {
        if (value == null) {
            return (S) this;
        }
        this.sqlContext.getConditions().add(new Condition(TableMetaRegistry.getGetterPropertyName(getter), SqlKeyword.NOT_LIKE.getKeyword(), value));
        return (S) this;
    }

    /**
     * 添加 IN 条件.
     *
     * @param getter 字段的 getter 方法引用
     * @param value  条件值集合
     * @param <R>    字段类型
     * @return 当前实例
     * @since 1.0.0
     */
    public <R> S in(GetterReference<T, R> getter, Collection<? extends R> value) {
        if (value == null) {
            return (S) this;
        }
        this.sqlContext.getConditions().add(new Condition(TableMetaRegistry.getGetterPropertyName(getter), SqlKeyword.IN.getKeyword(), value));
        return (S) this;
    }

    /**
     * 添加 NOT IN 条件.
     *
     * @param getter 字段的 getter 方法引用
     * @param value  条件值集合
     * @param <R>    字段类型
     * @return 当前实例
     * @since 1.0.0
     */
    public <R> S notIn(GetterReference<T, R> getter, Collection<? extends R> value) {
        if (value == null) {
            return (S) this;
        }
        this.sqlContext.getConditions().add(new Condition(TableMetaRegistry.getGetterPropertyName(getter), SqlKeyword.NOT_IN.getKeyword(), value));
        return (S) this;
    }

    /**
     * 添加 IS NULL 条件.
     *
     * @param getter 字段的 getter 方法引用
     * @return 当前实例
     * @since 1.0.0
     */
    public S isNull(GetterReference<T, ?> getter) {
        this.sqlContext.getConditions().add(new Condition(TableMetaRegistry.getGetterPropertyName(getter), SqlKeyword.IS_NULL.getKeyword(), true));
        return (S) this;
    }

    /**
     * 添加 IS NOT NULL 条件.
     *
     * @param getter 字段的 getter 方法引用
     * @return 当前实例
     * @since 1.0.0
     */
    public S isNotNull(GetterReference<T, ?> getter) {
        this.sqlContext.getConditions().add(new Condition(TableMetaRegistry.getGetterPropertyName(getter), SqlKeyword.IS_NOT_NULL.getKeyword(), true));
        return (S) this;
    }

    /**
     * 添加升序排序.
     *
     * @param getter 字段的 getter 方法引用
     * @return 当前实例
     * @since 1.0.0
     */
    public S orderByAsc(GetterReference<T, ?> getter) {
        this.sqlContext.getSorts().add(new Sort(TableMetaRegistry.getGetterPropertyName(getter), true));
        return (S) this;
    }

    /**
     * 添加降序排序.
     *
     * @param getter 字段的 getter 方法引用
     * @return 当前实例
     * @since 1.0.0
     */
    public S orderByDesc(GetterReference<T, ?> getter) {
        this.sqlContext.getSorts().add(new Sort(TableMetaRegistry.getGetterPropertyName(getter), false));
        return (S) this;
    }

    /**
     * 包含任意指定bit位
     *
     * @param getter 字段的 getter 方法引用
     * @param value  位码值
     * @param <R>    字段类型
     * @return 当前实例
     * @since 1.0.0
     */
    public <R> S bitAny(GetterReference<T, R> getter, R value) {
        if (value == null) {
            return (S) this;
        }
        this.sqlContext.getConditions().add(new Condition(TableMetaRegistry.getGetterPropertyName(getter), SqlKeyword.BIT_ANY.getKeyword(), value));
        return (S) this;
    }

    /**
     * 包含所有指定bit位
     *
     * @param getter 字段的 getter 方法引用
     * @param value  位码值
     * @param <R>    字段类型
     * @return 当前实例
     * @since 1.0.0
     */
    public <R> S bitAll(GetterReference<T, R> getter, R value) {
        if (value == null) {
            return (S) this;
        }
        this.sqlContext.getConditions().add(new Condition(TableMetaRegistry.getGetterPropertyName(getter), SqlKeyword.BIT_ALL.getKeyword(), value));
        return (S) this;
    }

    /**
     * 不包含指定bit位
     *
     * @param getter 字段的 getter 方法引用
     * @param value  位码值`
     * @param <R>    字段类型
     * @return 当前实例
     * @since 1.0.0
     */
    public <R> S bitNone(GetterReference<T, R> getter, R value) {
        if (value == null) {
            return (S) this;
        }
        this.sqlContext.getConditions().add(new Condition(TableMetaRegistry.getGetterPropertyName(getter), SqlKeyword.BIT_NONE.getKeyword(), value));
        return (S) this;
    }

}