package io.github.luminion.sqlbooster.model.sql.helper;

import io.github.luminion.sqlbooster.core.MethodReference;
import io.github.luminion.sqlbooster.model.enums.SqlKeyword;
import io.github.luminion.sqlbooster.model.sql.SqlCondition;
import io.github.luminion.sqlbooster.model.sql.SqlSort;
import io.github.luminion.sqlbooster.util.BoostUtils;

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
public interface LambdaHelper<T, S extends LambdaHelper<T, S>> extends BaseHelper<T> {

    /**
     * 添加一组 OR 连接的条件.
     *
     * @param consumer 用于构建 OR 条件的 Consumer
     * @return 当前实例
     * @since 1.0.0
     */
    S or(Consumer<S> consumer);

    /**
     * 添加等于 (EQ) 条件.
     *
     * @param getter 字段的 getter 方法引用
     * @param value  条件值
     * @param <R>    字段类型
     * @return 当前实例
     * @since 1.0.0
     */
    default <R> S eq(MethodReference<T, R> getter, R value) {
        if (value == null) {
            return (S) this;
        }
        getConditions().add(new SqlCondition(BoostUtils.getGetterPropertyName(getter), SqlKeyword.EQ.getKeyword(), value));
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
    default <R> S ne(MethodReference<T, R> getter, R value) {
        if (value == null) {
            return (S) this;
        }
        getConditions().add(new SqlCondition(BoostUtils.getGetterPropertyName(getter), SqlKeyword.NE.getKeyword(), value));
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
    default <R> S gt(MethodReference<T, R> getter, R value) {
        if (value == null) {
            return (S) this;
        }
        getConditions().add(new SqlCondition(BoostUtils.getGetterPropertyName(getter), SqlKeyword.GT.getKeyword(), value));
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
    default <R> S ge(MethodReference<T, R> getter, R value) {
        if (value == null) {
            return (S) this;
        }
        getConditions().add(new SqlCondition(BoostUtils.getGetterPropertyName(getter), SqlKeyword.GTE.getKeyword(), value));
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
    default <R> S lt(MethodReference<T, R> getter, R value) {
        if (value == null) {
            return (S) this;
        }
        getConditions().add(new SqlCondition(BoostUtils.getGetterPropertyName(getter), SqlKeyword.LT.getKeyword(), value));
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
    default <R> S le(MethodReference<T, R> getter, R value) {
        if (value == null) {
            return (S) this;
        }
        getConditions().add(new SqlCondition(BoostUtils.getGetterPropertyName(getter), SqlKeyword.LTE.getKeyword(), value));
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
    default <R> S like(MethodReference<T, R> getter, R value) {
        if (value == null) {
            return (S) this;
        }
        getConditions().add(new SqlCondition(BoostUtils.getGetterPropertyName(getter), SqlKeyword.LIKE.getKeyword(), value));
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
    default <R> S notLike(MethodReference<T, R> getter, R value) {
        if (value == null) {
            return (S) this;
        }
        getConditions().add(new SqlCondition(BoostUtils.getGetterPropertyName(getter), SqlKeyword.NOT_LIKE.getKeyword(), value));
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
    default <R> S in(MethodReference<T, R> getter, Collection<? extends R> value) {
        if (value == null) {
            return (S) this;
        }
        getConditions().add(new SqlCondition(BoostUtils.getGetterPropertyName(getter), SqlKeyword.IN.getKeyword(), value));
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
    default <R> S notIn(MethodReference<T, R> getter, Collection<? extends R> value) {
        if (value == null) {
            return (S) this;
        }
        getConditions().add(new SqlCondition(BoostUtils.getGetterPropertyName(getter), SqlKeyword.NOT_IN.getKeyword(), value));
        return (S) this;
    }

    /**
     * 添加 IS NULL 条件.
     *
     * @param getter 字段的 getter 方法引用
     * @return 当前实例
     * @since 1.0.0
     */
    default S isNull(MethodReference<T, ?> getter) {
        getConditions().add(new SqlCondition(BoostUtils.getGetterPropertyName(getter), SqlKeyword.IS_NULL.getKeyword(), true));
        return (S) this;
    }

    /**
     * 添加 IS NOT NULL 条件.
     *
     * @param getter 字段的 getter 方法引用
     * @return 当前实例
     * @since 1.0.0
     */
    default S isNotNull(MethodReference<T, ?> getter) {
        getConditions().add(new SqlCondition(BoostUtils.getGetterPropertyName(getter), SqlKeyword.IS_NOT_NULL.getKeyword(), true));
        return (S) this;
    }

    /**
     * 添加升序排序.
     *
     * @param getter 字段的 getter 方法引用
     * @return 当前实例
     * @since 1.0.0
     */
    default S orderByAsc(MethodReference<T, ?> getter) {
        getSorts().add(new SqlSort(BoostUtils.getGetterPropertyName(getter), true));
        return (S) this;
    }

    /**
     * 添加降序排序.
     *
     * @param getter 字段的 getter 方法引用
     * @return 当前实例
     * @since 1.0.0
     */
    default S orderByDesc(MethodReference<T, ?> getter) {
        getSorts().add(new SqlSort(BoostUtils.getGetterPropertyName(getter), false));
        return (S) this;
    }

    /**
     * 添加位运算包含条件.
     *
     * @param getter 字段的 getter 方法引用
     * @param value  位码值
     * @param <R>    字段类型
     * @return 当前实例
     * @since 1.0.0
     */
    default <R> S bitIn(MethodReference<T, R> getter, R value) {
        if (value == null) {
            return (S) this;
        }
        getConditions().add(new SqlCondition(BoostUtils.getGetterPropertyName(getter), SqlKeyword.BIT_IN.getKeyword(), value));
        return (S) this;
    }

    /**
     * 添加位运算不包含条件.
     *
     * @param getter 字段的 getter 方法引用
     * @param value  位码值
     * @param <R>    字段类型
     * @return 当前实例
     * @since 1.0.0
     */
    default <R> S bitNotIn(MethodReference<T, R> getter, R value) {
        if (value == null) {
            return (S) this;
        }
        getConditions().add(new SqlCondition(BoostUtils.getGetterPropertyName(getter), SqlKeyword.BIT_NOT_IN.getKeyword(), value));
        return (S) this;
    }

}