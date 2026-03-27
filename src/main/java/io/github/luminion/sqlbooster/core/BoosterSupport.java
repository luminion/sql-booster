package io.github.luminion.sqlbooster.core;

import io.github.luminion.sqlbooster.enums.SqlKeyword;
import io.github.luminion.sqlbooster.metadata.TableMetaRegistry;
import io.github.luminion.sqlbooster.model.BPage;
import io.github.luminion.sqlbooster.model.SqlContext;
import io.github.luminion.sqlbooster.model.query.Condition;
import org.springframework.util.ObjectUtils;

import java.io.Serializable;
import java.util.List;

/**
 * Booster 的核心支撑接口。
 * 提供三个核心查询方法的默认实现。
 *
 * @param <T> 实体类型
 * @param <V> 默认结果类型
 */
public interface BoosterSupport<T, V> extends BoosterOperations<T, V> {

    @Override
    default V voById(Serializable id) {
        if (ObjectUtils.isEmpty(id)) {
            throw new IllegalArgumentException("id can't be null");
        }
        String idPropertyName = TableMetaRegistry.getIdPropertyName(boosterEntityClass());
        if (ObjectUtils.isEmpty(idPropertyName)) {
            throw new IllegalStateException("can't find id property");
        }
        SqlContext<T> sqlContext = new SqlContext<>();
        sqlContext.getConditions().add(new Condition(idPropertyName, SqlKeyword.EQ.getSymbol(), id));
        return voUnique(sqlContext);
    }

    @Override
    default List<V> voList(SqlContext<T> sqlContext) {
        return doFetch(sqlContext, null);
    }

    @Override
    default BPage<V> voPage(SqlContext<T> sqlContext, long pageNum, long pageSize) {
        throw new UnsupportedOperationException("Current booster does not support paging.");
    }

    /**
     * 执行实际查询。
     *
     * @param sqlContext 查询上下文
     * @param page 分页插件对象，可为 null
     * @return 查询结果列表
     */
    List<V> doFetch(SqlContext<T> sqlContext, Object page);
}