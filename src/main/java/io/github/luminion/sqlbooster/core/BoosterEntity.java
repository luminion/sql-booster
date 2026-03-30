package io.github.luminion.sqlbooster.core;

import io.github.luminion.sqlbooster.metadata.BoosterRegistry;
import io.github.luminion.sqlbooster.util.GenericTypeUtils;

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
