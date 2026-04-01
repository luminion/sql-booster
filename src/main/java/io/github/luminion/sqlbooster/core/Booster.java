package io.github.luminion.sqlbooster.core;

import io.github.luminion.sqlbooster.model.BPage;
import io.github.luminion.sqlbooster.model.SqlContext;
import io.github.luminion.sqlbooster.util.GenericTypeUtils;

import java.io.Serializable;
import java.util.Collection;
import java.util.List;

/**
 * 用户侧最核心的查询入口。
 * `vo*` 方法直接执行查询，`lambdaBooster()` 返回可继续拼条件的链式入口。
 */
public interface Booster<T, V> {

    default Class<T> boosterEntityClass() {
        return GenericTypeUtils.resolveBoosterEntityClass(this);
    }

    default Class<V> boosterResultClass() {
        return GenericTypeUtils.resolveBoosterResultClass(this);
    }

    V voById(Serializable id);

    List<V> voByIds(Collection<? extends Serializable> ids);

    V voFirst(SqlContext<T> sqlContext);

    V voUnique(SqlContext<T> sqlContext);

    List<V> voList(SqlContext<T> sqlContext);

    BPage<V> voPage(SqlContext<T> sqlContext, long pageNum, long pageSize);

    /**
     * 返回链式查询入口。
     * 如果用户更习惯先写条件、最后再执行 `list/first/page`，应优先走这个入口。
     *
     * @since 1.2.0
     */
    default LambdaBooster<T, V> lambdaBooster() {
        return new LambdaBooster<>(this);
    }

    /**
     * @deprecated 请使用 {@link #lambdaBooster()}
     * @since 1.1.0
     */
    @Deprecated
    default LambdaBooster<T, V> lambdaBuilder() {
        return lambdaBooster();
    }

    /**
     * @deprecated 请使用 {@link #lambdaBooster()}
     */
    @Deprecated
    default LambdaBooster<T, V> sqlBuilder() {
        return lambdaBooster();
    }
}
