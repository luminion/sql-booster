package io.github.luminion.sqlbooster.core;

import io.github.luminion.sqlbooster.metadata.BoosterRegistry;

/**
 * Optional entity-side entrypoint for resolving the default booster.
 *
 * @param <T> entity self type
 * @param <V> default result type
 */
public interface BoosterModel<T extends BoosterModel<T, V>, V> {

    @SuppressWarnings("unchecked")
    default Booster<T, V> booster() {
        return BoosterRegistry.getRequiredDefault((Class<T>) this.getClass());
    }

    default LambdaBooster<T, V> lambdaBooster() {
        return booster().lambdaBooster();
    }
}