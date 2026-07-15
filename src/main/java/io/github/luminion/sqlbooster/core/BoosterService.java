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

    /**
     * 解析出真正执行 SQL 的 Booster。
     * <p>
     * 一旦 Service 自身被注册成对应键位的默认 Booster（{@link BoosterRegistry#registerBooster} 是公开 API），
     * 委托会指回 this，转发方法就会无限自递归直至 StackOverflow。这里显式拦截，给出可定位的错误而非栈溢出。
     */
    default Booster<T, V> requiredBooster() {
        Booster<T, V> booster = BoosterRegistry.getRequiredBooster(boosterEntityClass(), boosterResultClass());
        if (booster == this) {
            throw new IllegalStateException("BoosterService [" + getClass().getName()
                    + "] 被注册成了自身的默认 Booster，会导致无限递归委托；请提供独立的 Booster（如 Mapper）来执行 SQL。");
        }
        return booster;
    }

    @Override
    default V voById(Serializable id) {
        return requiredBooster().voById(id);
    }

    @Override
    default List<V> voList(SqlContext<T> sqlContext) {
        return requiredBooster().voList(sqlContext);
    }

    @Override
    default BPage<V> voPage(SqlContext<T> sqlContext, long pageNum, long pageSize) {
        return requiredBooster().voPage(sqlContext, pageNum, pageSize);
    }
}
