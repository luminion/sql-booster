package io.github.luminion.sqlbooster.core;

import io.github.luminion.sqlbooster.model.api.QueryParams;
import io.github.luminion.sqlbooster.model.enums.SqlKeyword;
import io.github.luminion.sqlbooster.model.api.Condition;
import io.github.luminion.sqlbooster.model.helper.SqlHelperBooster;
import io.github.luminion.sqlbooster.model.helper.SqlHelper;
import io.github.luminion.sqlbooster.model.helper.processor.SuffixProcessor;
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

    @Override
    default T toEntity(Object source) {
        return ReflectUtils.toTarget(source, BoostUtils.getEntityClass(this));
    }

    @Override
    default V toVo(Object source) {
        return ReflectUtils.toTarget(source, BoostUtils.getViewObjectClass(this));
    }

    @Override
    default void voPreProcess(QueryParams<T> queryParams) {
        // do nothing here, only for override
    }

    @Override
    default void voPostProcess(List<V> records, QueryParams<T> queryParams, BoosterPage<V> page) {
        // do nothing here, only for override
    }

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
    default V voFirst(QueryParams<T> queryParams) {
        List<V> vs = voList(queryParams);
        if (vs.isEmpty()) {
            return null;
        }
        return vs.get(0);
    }

    @Override
    default <R> R voFirst(QueryParams<T> queryParams, Class<R> targetType) {
        return ReflectUtils.toTarget(voFirst(queryParams), targetType);
    }

    @Override
    default Optional<V> voFirstOpt(QueryParams<T> queryParams) {
        return Optional.ofNullable(voFirst(queryParams));
    }

    @Override
    default <R> Optional<R> voFirstOpt(QueryParams<T> queryParams, Class<R> targetType) {
        return Optional.ofNullable(voFirst(queryParams, targetType));
    }

    @Override
    default V voUnique(QueryParams<T> queryParams) {
        List<V> vs = voList(queryParams);
        if (vs.isEmpty()) {
            return null;
        }
        if (vs.size() > 1) {
            throw new TooManyResultsException("error query => expected one but found " + vs.size());
        }
        return vs.get(0);
    }

    @Override
    default <R> R voUnique(QueryParams<T> queryParams, Class<R> targetType) {
        return ReflectUtils.toTarget(voUnique(queryParams), targetType);
    }

    @Override
    default Optional<V> voUniqueOpt(QueryParams<T> queryParams) {
        return Optional.ofNullable(voUnique(queryParams));
    }

    @Override
    default <R> Optional<R> voUniqueOpt(QueryParams<T> queryParams, Class<R> targetType) {
        return Optional.ofNullable(voUnique(queryParams, targetType));
    }

    @Override
    default List<V> voList() {
        return voList(null);
    }

    @Override
    default List<V> voList(QueryParams<T> queryParams) {
        voPreProcess(queryParams);

        Class<T> entityClass = BoostUtils.getEntityClass(this);
        SqlHelper<T> sqlHelper = SqlHelper.of(entityClass)
                .append(queryParams)
                .process(SuffixProcessor.of()::process);
        List<V> vs = selectByBooster(sqlHelper, null);

        voPostProcess(vs, sqlHelper, null);
        return vs;
    }

    @Override
    default <R> List<R> voList(QueryParams<T> queryParams, Class<R> targetType) {
        List<V> vs = voList(queryParams);
        return vs.stream()
                .map(v -> ReflectUtils.toTarget(v, targetType))
                .collect(Collectors.toList());
    }

    @Override
    default BoosterPage<V> voPage(QueryParams<T> queryParams, int pageNum, int pageSize) {
        return voPage(queryParams, (long) pageNum, pageSize);
    }

    @Override
    default BoosterPage<V> voPage(QueryParams<T> queryParams, long pageNum, long pageSize) {
        throw new UnsupportedOperationException("Not implemented.");
    }

    @Override
    default <R> BoosterPage<R> voPage(QueryParams<T> queryParams, int pageNum, int pageSize, Class<R> targetType) {
        return voPage(queryParams, (long) pageNum, pageSize, targetType);
    }

    @Override
    default <R> BoosterPage<R> voPage(QueryParams<T> queryParams, long pageNum, long pageSize, Class<R> targetType) {
        return voPage(queryParams, pageNum, pageSize).convertRecords(targetType);
    }

    /**
     * 获取 Lambda SQL 助手.
     *
     * @return SQL 助手
     * @since 1.0.0
     */
    default SqlHelperBooster<T, V> lambdaHelper() {
        return new SqlHelperBooster<>(this);
    }

    /**
     * 最终执行查询的方法.
     *
     * @param queryParams 查询条件
     * @param page    分页对象
     * @return 查询结果列表
     * @since 1.0.0
     */
    List<V> selectByBooster(QueryParams<T> queryParams, Object page);
}
