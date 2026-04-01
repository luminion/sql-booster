package io.github.luminion.sqlbooster.core;

import io.github.luminion.sqlbooster.metadata.BoosterRegistry;
import io.github.luminion.sqlbooster.model.BPage;
import io.github.luminion.sqlbooster.model.SqlContext;

import java.io.Serializable;
import java.util.List;

/**
 * Service 层入口。
 * 自己不执行 SQL，只负责把调用转发给注册到 {@link BoosterRegistry} 里的默认 Booster。
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
