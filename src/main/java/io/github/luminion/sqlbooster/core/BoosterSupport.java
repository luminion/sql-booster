package io.github.luminion.sqlbooster.core;

import io.github.luminion.sqlbooster.model.BoosterPage;
import io.github.luminion.sqlbooster.model.BoosterParam;
import io.github.luminion.sqlbooster.model.query.Condition;
import io.github.luminion.sqlbooster.builder.SqlBuilder;
import io.github.luminion.sqlbooster.enums.SqlKeyword;
import io.github.luminion.sqlbooster.util.TableInfoUtils;
import io.github.luminion.sqlbooster.util.ReflectUtils;
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
public interface BoosterSupport<T, V> extends BoosterApi<T, V> {

    //@Override
    //default T toEntity(Object source) {
    //    return ReflectUtils.toTarget(source, TableInfoUtils.getEntityClass(this));
    //}
    //
    //@Override
    //default V toVo(Object source) {
    //    return ReflectUtils.toTarget(source, TableInfoUtils.getViewObjectClass(this));
    //}
    //
    //@Override
    //default void voPreProcess(BoosterParam<T> boosterParam) {
    //    // do nothing here, only for override
    //}
    //
    //@Override
    //default void voPostProcess(List<V> records, BoosterParam<T> boosterParam, BoosterPage<V> page) {
    //    // do nothing here, only for override
    //}

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
        SqlBuilder<T> sqlBuilder = SqlBuilder.of(this).append(condition);
        return voUnique(sqlBuilder);
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
        SqlBuilder<T> sqlBuilder = SqlBuilder.of(this).append(condition);
        return voList(sqlBuilder);
    }

    @Override
    default <R> List<R> voListByIds(Collection<? extends Serializable> ids, Class<R> targetType) {
        List<V> vs = voListByIds(ids);
        return vs.stream()
                .map(v -> ReflectUtils.toTarget(v, targetType))
                .collect(Collectors.toList());
    }

    @Override
    default V voFirst(BoosterParam<T> boosterParam) {
        List<V> vs = voList(boosterParam);
        if (vs.isEmpty()) {
            return null;
        }
        return vs.get(0);
    }

    @Override
    default <R> R voFirst(BoosterParam<T> boosterParam, Class<R> targetType) {
        return ReflectUtils.toTarget(voFirst(boosterParam), targetType);
    }

    @Override
    default Optional<V> voFirstOpt(BoosterParam<T> boosterParam) {
        return Optional.ofNullable(voFirst(boosterParam));
    }

    @Override
    default <R> Optional<R> voFirstOpt(BoosterParam<T> boosterParam, Class<R> targetType) {
        return Optional.ofNullable(voFirst(boosterParam, targetType));
    }

    @Override
    default V voUnique(BoosterParam<T> boosterParam) {
        List<V> vs = voList(boosterParam);
        if (vs.isEmpty()) {
            return null;
        }
        if (vs.size() > 1) {
            throw new TooManyResultsException("error query => expected one but found 0" + vs.size());
        }
        return vs.get(0);
    }

    @Override
    default <R> R voUnique(BoosterParam<T> boosterParam, Class<R> targetType) {
        return ReflectUtils.toTarget(voUnique(boosterParam), targetType);
    }

    @Override
    default Optional<V> voUniqueOpt(BoosterParam<T> boosterParam) {
        return Optional.ofNullable(voUnique(boosterParam));
    }

    @Override
    default <R> Optional<R> voUniqueOpt(BoosterParam<T> boosterParam, Class<R> targetType) {
        return Optional.ofNullable(voUnique(boosterParam, targetType));
    }

    @Override
    default List<V> voList() {
        return voList(null);
    }

    @Override
    default List<V> voList(BoosterParam<T> boosterParam) {
        return selectByBooster(boosterParam, null);
    }

    @Override
    default <R> List<R> voList(BoosterParam<T> boosterParam, Class<R> targetType) {
        List<V> vs = voList(boosterParam);
        return vs.stream()
                .map(v -> ReflectUtils.toTarget(v, targetType))
                .collect(Collectors.toList());
    }

    @Override
    default BoosterPage<V> voPage(BoosterParam<T> boosterParam, int pageNum, int pageSize) {
        return voPage(boosterParam, (long) pageNum, pageSize);
    }

    @Override
    default BoosterPage<V> voPage(BoosterParam<T> boosterParam, long pageNum, long pageSize) {
        throw new UnsupportedOperationException("Not implemented.");
    }

    @Override
    default <R> BoosterPage<R> voPage(BoosterParam<T> boosterParam, int pageNum, int pageSize, Class<R> targetType) {
        return voPage(boosterParam, (long) pageNum, pageSize, targetType);
    }

    @Override
    default <R> BoosterPage<R> voPage(BoosterParam<T> boosterParam, long pageNum, long pageSize, Class<R> targetType) {
        return voPage(boosterParam, pageNum, pageSize).convertRecords(targetType);
    }

    /**
     * 最终执行查询的方法.
     *
     * @param boosterParam 查询条件
     * @param page         分页对象
     * @return 查询结果列表
     * @since 1.0.0
     */
    List<V> selectByBooster(BoosterParam<T> boosterParam, Object page);
}
