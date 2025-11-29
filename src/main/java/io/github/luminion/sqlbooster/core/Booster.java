package io.github.luminion.sqlbooster.core;

import io.github.luminion.sqlbooster.builder.SqlBuilderWrapper;
import io.github.luminion.sqlbooster.model.BPage;
import io.github.luminion.sqlbooster.model.SqlContext;

import java.io.Serializable;
import java.util.Collection;
import java.util.List;
import java.util.Optional;

/**
 * 提供灵活的视图对象（VO）查询及类型转换功能的核心接口。
 *
 * <h3>方法命名约定:</h3>
 * <ul>
 *   <li>{@code voById} - 根据 ID 查询。</li>
 *   <li>{@code voByIds} - 根据 ID 集合批量查询。</li>
 *   <li>{@code voFirst} - 查询第一条记录（如果存在多条，返回第一条）。</li>
 *   <li>{@code voUnique} - 查询唯一记录（如果存在多条，会抛出异常）。</li>
 *   <li>{@code voList} - 查询记录列表。</li>
 *   <li>{@code voPage} - 分页查询记录列表。</li>
 * </ul>
 *
 * @param <T> 数据库实体类型
 * @param <V> 返回的视图对象 (VO) 类型
 */
public interface Booster<T, V> {

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

    /**
     * 获取一个 SQL 构建器实例。
     *
     * @return 用于链式调用的 SQL 构建器
     */
    SqlBuilderWrapper<T, V> sqlBuilder();
}
