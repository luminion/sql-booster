package io.github.luminion.sqlbooster.core;

import io.github.luminion.sqlbooster.builder.LambdaBooster;
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
 * <li>{@code voById} - 根据 ID 查询。</li>
 * <li>{@code voByIds} - 根据 ID 集合批量查询。</li>
 * <li>{@code voFirst} - 查询第一条记录（如果存在多条，返回第一条）。</li>
 * <li>{@code voUnique} - 查询唯一记录（如果存在多条，会抛出异常）。</li>
 * <li>{@code voList} - 查询记录列表。</li>
 * <li>{@code voPage} - 分页查询记录列表。</li>
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
     * 开启一个基于 Lambda 语法的增强查询链。
     * <p>
     * 通过返回的 {@link LambdaBooster} 对象，你可以安全、优雅地链式拼接动态 SQL 条件，
     * 并在链条末端直接执行查询，获取实体或对应的 VO 结果。
     * </p>
     *
     * @return 支持链式调用和终端查询执行的 {@code LambdaBooster} 实例
     */
    default LambdaBooster<T, V> lambdaBooster() {
        return new LambdaBooster<>(this);
    }

    /**
     * 仅作兼容性处理, 
     * @deprecated 使用{@link #lambdaBooster()}代替
     */
    @Deprecated
    default LambdaBooster<T, V> lambdaBuilder() {
        return lambdaBooster();
    }

    /**
     * 仅作兼容性处理, 
     * @deprecated 使用{@link #lambdaBooster()}代替
     */
    @Deprecated
    default LambdaBooster<T, V> sqlBuilder(){
        return lambdaBooster();
    }
    
    
}
