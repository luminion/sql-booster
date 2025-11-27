package io.github.luminion.sqlbooster.core;

import io.github.luminion.sqlbooster.model.api.Condition;
import io.github.luminion.sqlbooster.model.enums.SqlKeyword;
import io.github.luminion.sqlbooster.model.helper.SqlHelper;
import io.github.luminion.sqlbooster.util.BoostUtils;
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
public interface BoosterEngine<T, V> extends BoosterCore<T, V> {

    //@Override
    //default T toEntity(Object source) {
    //    return ReflectUtils.toTarget(source, BoostUtils.getEntityClass(this));
    //}
    //
    //@Override
    //default V toVo(Object source) {
    //    return ReflectUtils.toTarget(source, BoostUtils.getViewObjectClass(this));
    //}
    //
    //@Override
    //default void voPreProcess(QueryParam<T> boosterParam) {
    //    // do nothing here, only for override
    //}
    //
    //@Override
    //default void voPostProcess(List<V> records, QueryParam<T> boosterParam, BoosterPage<V> page) {
    //    // do nothing here, only for override
    //}

    @Override
    default V voById(Serializable id) {
        if (ObjectUtils.isEmpty(id)) {
            throw new NullPointerException("id can't be null");
        }
        Class<T> clazz = BoostUtils.getEntityClass(this);
        String keyProperty = BoostUtils.getIdPropertyName(clazz);
        if (ObjectUtils.isEmpty(keyProperty)) {
            throw new IllegalStateException("can't find id property");
        }
        Condition condition = new Condition(keyProperty, SqlKeyword.EQ.getKeyword(), id);
        SqlHelper<T> sqlHelper = SqlHelper.of(this).append(condition);
        return voUnique(sqlHelper);
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
        Class<T> entityClass = BoostUtils.getEntityClass(this);
        String idPropertyName = BoostUtils.getIdPropertyName(entityClass);
        Condition condition = new Condition(idPropertyName, SqlKeyword.IN.getKeyword(), ids);
        SqlHelper<T> sqlHelper = SqlHelper.of(this).append(condition);
        return voList(sqlHelper);
    }

    @Override
    default <R> List<R> voListByIds(Collection<? extends Serializable> ids, Class<R> targetType) {
        List<V> vs = voListByIds(ids);
        return vs.stream()
                .map(v -> ReflectUtils.toTarget(v, targetType))
                .collect(Collectors.toList());
    }

    @Override
    default V voFirst(QueryParam<T> queryParam) {
        List<V> vs = voList(queryParam);
        if (vs.isEmpty()) {
            return null;
        }
        return vs.get(0);
    }

    @Override
    default <R> R voFirst(QueryParam<T> queryParam, Class<R> targetType) {
        return ReflectUtils.toTarget(voFirst(queryParam), targetType);
    }

    @Override
    default Optional<V> voFirstOpt(QueryParam<T> queryParam) {
        return Optional.ofNullable(voFirst(queryParam));
    }

    @Override
    default <R> Optional<R> voFirstOpt(QueryParam<T> queryParam, Class<R> targetType) {
        return Optional.ofNullable(voFirst(queryParam, targetType));
    }

    @Override
    default V voUnique(QueryParam<T> queryParam) {
        List<V> vs = voList(queryParam);
        if (vs.isEmpty()) {
            return null;
        }
        if (vs.size() > 1) {
            throw new TooManyResultsException("error query => expected one but found 0" + vs.size());
        }
        return vs.get(0);
    }

    @Override
    default <R> R voUnique(QueryParam<T> queryParam, Class<R> targetType) {
        return ReflectUtils.toTarget(voUnique(queryParam), targetType);
    }

    @Override
    default Optional<V> voUniqueOpt(QueryParam<T> queryParam) {
        return Optional.ofNullable(voUnique(queryParam));
    }

    @Override
    default <R> Optional<R> voUniqueOpt(QueryParam<T> queryParam, Class<R> targetType) {
        return Optional.ofNullable(voUnique(queryParam, targetType));
    }

    @Override
    default List<V> voList() {
        return voList(null);
    }

    @Override
    default List<V> voList(QueryParam<T> queryParam) {
        return selectByBooster(queryParam, null);
    }

    @Override
    default <R> List<R> voList(QueryParam<T> queryParam, Class<R> targetType) {
        List<V> vs = voList(queryParam);
        return vs.stream()
                .map(v -> ReflectUtils.toTarget(v, targetType))
                .collect(Collectors.toList());
    }

    @Override
    default BoosterPage<V> voPage(QueryParam<T> queryParam, int pageNum, int pageSize) {
        return voPage(queryParam, (long) pageNum, pageSize);
    }

    @Override
    default BoosterPage<V> voPage(QueryParam<T> queryParam, long pageNum, long pageSize) {
        throw new UnsupportedOperationException("Not implemented.");
    }

    @Override
    default <R> BoosterPage<R> voPage(QueryParam<T> queryParam, int pageNum, int pageSize, Class<R> targetType) {
        return voPage(queryParam, (long) pageNum, pageSize, targetType);
    }

    @Override
    default <R> BoosterPage<R> voPage(QueryParam<T> queryParam, long pageNum, long pageSize, Class<R> targetType) {
        return voPage(queryParam, pageNum, pageSize).convertRecords(targetType);
    }

    /**
     * 最终执行查询的方法.
     *
     * @param queryParam 查询条件
     * @param page         分页对象
     * @return 查询结果列表
     * @since 1.0.0
     */
    List<V> selectByBooster(QueryParam<T> queryParam, Object page);
}
