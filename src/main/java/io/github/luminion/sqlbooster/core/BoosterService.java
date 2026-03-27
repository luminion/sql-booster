package io.github.luminion.sqlbooster.core;

import io.github.luminion.sqlbooster.metadata.BoosterRegistry;
import io.github.luminion.sqlbooster.model.BPage;
import io.github.luminion.sqlbooster.model.SqlContext;

import java.io.Serializable;
import java.util.List;

/**
 * Service 层的 booster 薄封装接口。
 * 仅负责从注册中心获取默认 booster 并转发查询调用。
 *
 * @param <T> 实体类型
 * @param <V> 默认结果类型
 */
public interface BoosterService<T, V> extends BoosterOperations<T, V> {

    @Override
    default V voById(Serializable id) {
        return BoosterRegistry.getRequiredBooster(boosterEntityClass(), boosterResultClass()).voById(id);
    }

    @Override
    default List<V> voList(SqlContext<T> sqlContext) {
        return BoosterRegistry.getRequiredBooster(boosterEntityClass(), boosterResultClass()).voList(sqlContext);
    }

    @Override
    default BPage<V> voPage(SqlContext<T> sqlContext, long pageNum, long pageSize) {
        return BoosterRegistry.getRequiredBooster(boosterEntityClass(), boosterResultClass()).voPage(sqlContext, pageNum,
                pageSize);
    }
}