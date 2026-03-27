package io.github.luminion.sqlbooster.core;

import io.github.luminion.sqlbooster.model.BPage;
import io.github.luminion.sqlbooster.model.SqlContext;
import io.github.luminion.sqlbooster.util.GenericTypeUtils;

import java.io.Serializable;
import java.util.Collection;
import java.util.List;

/**
 * sql-booster 的核心查询接口，定义实体类型到默认结果类型的查询契约。
 *
 * @param <T> 实体类型
 * @param <V> 默认结果类型
 */
public interface Booster<T, V> {

    default Class<T> boosterEntityClass() {
        return GenericTypeUtils.resolveBoosterEntityClass(this);
    }

    default Class<V> boosterResultClass() {
        return GenericTypeUtils.resolveBoosterResultClass(this);
    }

    /**
     * 根据主键查询单条结果。
     *
     * @param id 主键值
     * @return 查询结果，不存在时返回 null
     */
    V voById(Serializable id);

    /**
     * 根据主键集合查询结果列表。
     *
     * @param ids 主键集合
     * @return 查询结果列表
     */
    List<V> voByIds(Collection<? extends Serializable> ids);

    /**
     * 按查询上下文查询首条结果。
     *
     * @param sqlContext 查询上下文
     * @return 首条结果，不存在时返回 null
     */
    V voFirst(SqlContext<T> sqlContext);

    /**
     * 按查询上下文查询唯一结果。
     *
     * @param sqlContext 查询上下文
     * @return 唯一结果，不存在时返回 null
     */
    V voUnique(SqlContext<T> sqlContext);

    /**
     * 按查询上下文查询结果列表。
     *
     * @param sqlContext 查询上下文
     * @return 查询结果列表
     */
    List<V> voList(SqlContext<T> sqlContext);

    /**
     * 按查询上下文执行分页查询。
     *
     * @param sqlContext 查询上下文
     * @param pageNum 页码
     * @param pageSize 每页大小
     * @return 分页结果
     */
    BPage<V> voPage(SqlContext<T> sqlContext, long pageNum, long pageSize);

    /**
     * 获取 Lambda 链式查询 booster。
     *
     * @return 链式 booster
     * @since 1.2.0
     */
    default LambdaBooster<T, V> lambdaBooster() {
        return new LambdaBooster<>(this);
    }

    /**
     * 兼容旧版命名。
     *
     * @deprecated 请使用 {@link #lambdaBooster()}
     * @since 1.1.0
     */
    @Deprecated
    default LambdaBooster<T, V> lambdaBuilder() {
        return lambdaBooster();
    }

    /**
     * 兼容旧版命名。
     *
     * @deprecated 请使用 {@link #lambdaBooster()}
     */
    @Deprecated
    default LambdaBooster<T, V> sqlBuilder() {
        return lambdaBooster();
    }
}