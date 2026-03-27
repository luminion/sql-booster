package io.github.luminion.sqlbooster.core;

import io.github.luminion.sqlbooster.model.BPage;
import io.github.luminion.sqlbooster.model.SqlContext;
import io.github.luminion.sqlbooster.util.GenericTypeUtils;

import java.io.Serializable;
import java.util.Collection;
import java.util.List;
import java.util.Optional;

/**
 * 提供灵活的视图对象（VO）查询及类型转换功能的核心接口。
 *
 * @param <T> 数据库实体类型
 * @param <V> 返回的目标对象类型
 */
public interface Booster<T, V> {

    default Class<T> entityClass() {
        return GenericTypeUtils.resolveBoosterEntityClass(this);
    }

    default Class<V> resultClass() {
        return GenericTypeUtils.resolveBoosterVoClass(this);
    }

    V voById(Serializable id);

    <R> R voById(Serializable id, Class<R> targetType);

    Optional<V> voByIdOpt(Serializable id);

    <R> Optional<R> voByIdOpt(Serializable id, Class<R> targetType);

    List<V> voListByIds(Collection<? extends Serializable> ids);

    <R> List<R> voListByIds(Collection<? extends Serializable> ids, Class<R> targetType);

    V voFirst(SqlContext<T> sqlContext);

    <R> R voFirst(SqlContext<T> sqlContext, Class<R> targetType);

    Optional<V> voFirstOpt(SqlContext<T> sqlContext);

    <R> Optional<R> voFirstOpt(SqlContext<T> sqlContext, Class<R> targetType);

    V voUnique(SqlContext<T> sqlContext);

    <R> R voUnique(SqlContext<T> sqlContext, Class<R> targetType);

    Optional<V> voUniqueOpt(SqlContext<T> sqlContext);

    <R> Optional<R> voUniqueOpt(SqlContext<T> sqlContext, Class<R> targetType);

    List<V> voList();

    List<V> voList(SqlContext<T> sqlContext);

    <R> List<R> voList(SqlContext<T> sqlContext, Class<R> targetType);

    BPage<V> voPage(SqlContext<T> sqlContext, int pageNum, int pageSize);

    BPage<V> voPage(SqlContext<T> sqlContext, long pageNum, long pageSize);

    <R> BPage<R> voPage(SqlContext<T> sqlContext, int pageNum, int pageSize, Class<R> targetType);

    <R> BPage<R> voPage(SqlContext<T> sqlContext, long pageNum, long pageSize, Class<R> targetType);

    default LambdaBooster<T, V> lambdaBooster() {
        return new LambdaBooster<>(this);
    }

    /**
     * 仅作兼容性处理。
     *
     * @deprecated 使用 {@link #lambdaBooster()} 代替
     */
    @Deprecated
    default LambdaBooster<T, V> lambdaBuilder() {
        return lambdaBooster();
    }

    /**
     * 仅作兼容性处理。
     *
     * @deprecated 使用 {@link #lambdaBooster()} 代替
     */
    @Deprecated
    default LambdaBooster<T, V> sqlBuilder() {
        return lambdaBooster();
    }
}
