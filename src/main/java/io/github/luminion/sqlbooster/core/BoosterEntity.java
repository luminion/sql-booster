package io.github.luminion.sqlbooster.core;

import io.github.luminion.sqlbooster.metadata.BoosterRegistry;
import io.github.luminion.sqlbooster.util.GenericTypeUtils;

/**
 * 实体侧的默认 Booster 获取入口。
 *
 * @param <T> 实体自身类型
 * @param <V> 默认结果类型
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

    default LambdaBooster<T, V> lambdaBooster() {
        return booster().lambdaBooster().fromBean(this);
    }
}