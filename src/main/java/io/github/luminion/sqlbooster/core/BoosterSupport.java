package io.github.luminion.sqlbooster.core;

import io.github.luminion.sqlbooster.builder.SqlBuilderWrapper;
import io.github.luminion.sqlbooster.enums.SqlKeyword;
import io.github.luminion.sqlbooster.model.BPage;
import io.github.luminion.sqlbooster.model.SqlContext;
import io.github.luminion.sqlbooster.model.query.Condition;
import io.github.luminion.sqlbooster.util.ReflectUtils;
import io.github.luminion.sqlbooster.util.TableInfoUtils;
import org.apache.ibatis.exceptions.TooManyResultsException;
import org.springframework.util.ObjectUtils;

import java.io.Serializable;
import java.util.Collection;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

/**
 * Boost 核心引擎，提供 VO 查询能力的默认实现.
 *
 * @param <T> 数据库实体的类型。
 * @param <V> 要返回的视图对象 (VO) 的类型。
 * @author luminion
 * @since 1.0.0
 */
public interface BoosterSupport<T, V> extends Booster<T, V> {

    @Override
    default V voById(Serializable id) {
        if (ObjectUtils.isEmpty(id)) {
            throw new NullPointerException("id can't be null");
        }
        Class<T> clazz = TableInfoUtils.getEntityClass(this);
        String keyProperty = TableInfoUtils.getIdPropertyName(clazz);
        if (ObjectUtils.isEmpty(keyProperty)) {
            throw new IllegalStateException("can't find id property");
        }
        Condition condition = new Condition(keyProperty, SqlKeyword.EQ.getKeyword(), id);
        SqlContext<T> sqlContext = new SqlContext<>();
        sqlContext.getConditions().add(condition);
        return voUnique(sqlContext);
    }

    @Override
    default <R> R voById(Serializable id, Class<R> targetType) {
        V v = voById(id);
        if (ObjectUtils.isEmpty(v)) {
            return null;
        }
        return ReflectUtils.toTarget(v, targetType);
    }

    @Override
    default Optional<V> voByIdOpt(Serializable id) {
        return Optional.ofNullable(voById(id));
    }

    @Override
    default <R> Optional<R> voByIdOpt(Serializable id, Class<R> targetType) {
        return Optional.ofNullable(voById(id, targetType));
    }

    @Override
    default List<V> voListByIds(Collection<? extends Serializable> ids) {
        Class<T> entityClass = TableInfoUtils.getEntityClass(this);
        String idPropertyName = TableInfoUtils.getIdPropertyName(entityClass);
        Condition condition = new Condition(idPropertyName, SqlKeyword.IN.getKeyword(), ids);
        SqlContext<T> sqlContext = new SqlContext<>();
        sqlContext.getConditions().add(condition);
        return voList(sqlContext);
    }

    @Override
    default <R> List<R> voListByIds(Collection<? extends Serializable> ids, Class<R> targetType) {
        List<V> vs = voListByIds(ids);
        return vs.stream()
                .map(v -> ReflectUtils.toTarget(v, targetType))
                .collect(Collectors.toList());
    }

    @Override
    default V voFirst(SqlContext<T> sqlContext) {
        List<V> vs = voList(sqlContext);
        if (vs.isEmpty()) {
            return null;
        }
        return vs.get(0);
    }

    @Override
    default <R> R voFirst(SqlContext<T> sqlContext, Class<R> targetType) {
        return ReflectUtils.toTarget(voFirst(sqlContext), targetType);
    }

    @Override
    default Optional<V> voFirstOpt(SqlContext<T> sqlContext) {
        return Optional.ofNullable(voFirst(sqlContext));
    }

    @Override
    default <R> Optional<R> voFirstOpt(SqlContext<T> sqlContext, Class<R> targetType) {
        return Optional.ofNullable(voFirst(sqlContext, targetType));
    }

    @Override
    default V voUnique(SqlContext<T> sqlContext) {
        List<V> vs = voList(sqlContext);
        if (vs.isEmpty()) {
            return null;
        }
        if (vs.size() > 1) {
            throw new TooManyResultsException("error query => expected one but found 0" + vs.size());
        }
        return vs.get(0);
    }

    @Override
    default <R> R voUnique(SqlContext<T> sqlContext, Class<R> targetType) {
        return ReflectUtils.toTarget(voUnique(sqlContext), targetType);
    }

    @Override
    default Optional<V> voUniqueOpt(SqlContext<T> sqlContext) {
        return Optional.ofNullable(voUnique(sqlContext));
    }

    @Override
    default <R> Optional<R> voUniqueOpt(SqlContext<T> sqlContext, Class<R> targetType) {
        return Optional.ofNullable(voUnique(sqlContext, targetType));
    }

    @Override
    default List<V> voList() {
        return voList(null);
    }

    @Override
    default List<V> voList(SqlContext<T> sqlContext) {
        return selectByBooster(sqlContext, null);
    }

    @Override
    default <R> List<R> voList(SqlContext<T> sqlContext, Class<R> targetType) {
        List<V> vs = voList(sqlContext);
        return vs.stream()
                .map(v -> ReflectUtils.toTarget(v, targetType))
                .collect(Collectors.toList());
    }

    @Override
    default BPage<V> voPage(SqlContext<T> sqlContext, int pageNum, int pageSize) {
        return voPage(sqlContext, (long) pageNum, pageSize);
    }

    @Override
    default BPage<V> voPage(SqlContext<T> sqlContext, long pageNum, long pageSize) {
        throw new UnsupportedOperationException("Not implemented.");
    }

    @Override
    default <R> BPage<R> voPage(SqlContext<T> sqlContext, int pageNum, int pageSize, Class<R> targetType) {
        return voPage(sqlContext, (long) pageNum, pageSize, targetType);
    }

    @Override
    default <R> BPage<R> voPage(SqlContext<T> sqlContext, long pageNum, long pageSize, Class<R> targetType) {
        return voPage(sqlContext, pageNum, pageSize).convertRecords(targetType);
    }
    
    @Override
    default SqlBuilderWrapper<T, V> sqlBuilder() {
        return new SqlBuilderWrapper<>(this);
    }

    /**
     * 最终执行查询的方法.
     *
     * @param sqlContext 查询条件
     * @param page         分页对象
     * @return 查询结果列表
     * @since 1.0.0
     */
    List<V> selectByBooster(SqlContext<T> sqlContext, Object page);
}
