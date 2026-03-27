package io.github.luminion.sqlbooster.util;

import io.github.luminion.sqlbooster.core.Booster;
import io.github.luminion.sqlbooster.core.BoosterEntity;
import io.github.luminion.sqlbooster.core.BoosterService;
import org.springframework.core.ResolvableType;
import org.springframework.util.ClassUtils;

public abstract class GenericTypeUtils {

    public static Class<?>[] resolveTypeArguments(Class<?> clazz, Class<?> genericIfc) {
        ResolvableType type = ResolvableType.forClass(clazz).as(genericIfc);
        ResolvableType[] generics = type.getGenerics();
        if (generics.length == 0) {
            return null;
        }
        Class<?>[] result = new Class<?>[generics.length];
        for (int i = 0; i < generics.length; i++) {
            result[i] = generics[i].resolve();
        }
        return result;
    }

    @SuppressWarnings("unchecked")
    public static <T, V> Class<T> resolveBoosterEntityClass(Booster<T, V> booster) {
        return (Class<T>) resolveRequiredTypeArgument(ClassUtils.getUserClass(booster.getClass()), Booster.class, 0,
                "booster entity");
    }

    @SuppressWarnings("unchecked")
    public static <T, V> Class<V> resolveBoosterResultClass(Booster<T, V> booster) {
        return (Class<V>) resolveRequiredTypeArgument(ClassUtils.getUserClass(booster.getClass()), Booster.class, 1,
                "booster result");
    }

    @SuppressWarnings("unchecked")
    public static <T extends BoosterEntity<T, V>, V> Class<T> resolveBoosterHolderEntityClass(BoosterEntity<T, V> entity) {
        return (Class<T>) resolveRequiredTypeArgument(ClassUtils.getUserClass(entity.getClass()), BoosterEntity.class, 0,
                "booster holder entity");
    }

    @SuppressWarnings("unchecked")
    public static <T extends BoosterEntity<T, V>, V> Class<V> resolveBoosterHolderResultClass(BoosterEntity<T, V> entity) {
        return (Class<V>) resolveRequiredTypeArgument(ClassUtils.getUserClass(entity.getClass()), BoosterEntity.class, 1,
                "booster holder result");
    }

    private static Class<?> resolveRequiredTypeArgument(Class<?> sourceClass, Class<?> genericIfc, int index,
                                                        String label) {
        ResolvableType type = ResolvableType.forClass(sourceClass).as(genericIfc);
        Class<?> resolved = type.getGeneric(index).resolve();
        if (resolved == null) {
            throw new IllegalStateException("Unable to resolve " + label + " type from " + sourceClass.getName());
        }
        return resolved;
    }
}