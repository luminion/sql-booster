package io.github.luminion.sqlbooster.core;

import io.github.luminion.sqlbooster.metadata.BoosterRegistry;
import io.github.luminion.sqlbooster.util.GenericTypeUtils;

/**
 * 给实体类挂一个“就地发起查询”的入口。
 * 适合 `new Entity().lambdaBooster()` 这种从查询对象直接起手的用法。
 */
public interface BoosterEntity<T extends BoosterEntity<T, V>, V> {

    default Class<T> boosterEntityClass() {
        return GenericTypeUtils.resolveBoosterHolderEntityClass(this);
    }

    default Class<V> boosterResultClass() {
        return GenericTypeUtils.resolveBoosterHolderResultClass(this);
    }

    default Booster<T, V> booster() {
        return BoosterRegistry.getRequiredBooster(boosterEntityClass(), boosterResultClass());
    }

    /**
     * 先把当前实体转成查询条件，再继续链式追加条件。
     */
    default LambdaBooster<T, V> lambdaBooster() {
        return booster().lambdaBooster().fromBean(this);
    }
}
