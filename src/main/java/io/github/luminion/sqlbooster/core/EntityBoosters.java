package io.github.luminion.sqlbooster.core;

import io.github.luminion.sqlbooster.builder.LambdaBooster;
import io.github.luminion.sqlbooster.util.GenericTypeUtils;

/**
 * 实体 Booster 获取入口。
 */
public abstract class EntityBoosters {

    @SuppressWarnings("unchecked")
    public static <T extends BoosterModel<T, V>, V> Booster<T, V> booster(T entity) {
        return booster((Class<T>) entity.getClass());
    }

    public static <T extends BoosterModel<T, V>, V> Booster<T, V> booster(Class<T> entityClass) {
        Class<V> targetType = GenericTypeUtils.resolveBoosterModelResultClass(entityClass);
        return booster(entityClass, targetType);
    }

    @SuppressWarnings("unchecked")
    public static <T extends BoosterModel<T, V>, V> LambdaBooster<T, V> lambda(T entity) {
        return lambda((Class<T>) entity.getClass());
    }

    public static <T extends BoosterModel<T, V>, V> LambdaBooster<T, V> lambda(Class<T> entityClass) {
        return booster(entityClass).lambdaBooster();
    }

    @SuppressWarnings("unchecked")
    public static <T extends BoosterModel<T, ?>, R> Booster<T, R> booster(T entity, Class<R> targetType) {
        return booster((Class<T>) entity.getClass(), targetType);
    }

    @SuppressWarnings("unchecked")
    public static <T extends BoosterModel<T, ?>, R> LambdaBooster<T, R> lambda(T entity, Class<R> targetType) {
        return lambda((Class<T>) entity.getClass(), targetType);
    }

    public static <T, R> Booster<T, R> booster(Class<T> entityClass, Class<R> targetType) {
        return BoosterRegistry.getRequired(entityClass, targetType);
    }

    public static <T, R> LambdaBooster<T, R> lambda(Class<T> entityClass, Class<R> targetType) {
        return booster(entityClass, targetType).lambdaBooster();
    }
}