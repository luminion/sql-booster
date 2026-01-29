package io.github.luminion.sqlbooster.core;

import io.github.luminion.sqlbooster.model.SqlContext;

import java.util.List;

/**
 * Booster 查询生命周期拦截器，允许在查询前后对请求上下文和结果集进行干预。
 *
 * @param <T> 实体类型
 * @param <V> 视图对象类型
 * @author luminion
 * @since 1.0.0
 */
public interface BoosterInterceptor<T, V> {

    /**
     * 查询执行前处理
     *
     * @param context 查询上下文（可修改条件、排序等）
     */
    default void preHandle(SqlContext<T> context) {
    }

    /**
     * 查询执行后处理
     *
     * @param context    查询上下文
     * @param resultList 查询返回的结果集（已转 VO）
     */
    default void postHandle(SqlContext<T> context, List<V> resultList) {
    }
}
