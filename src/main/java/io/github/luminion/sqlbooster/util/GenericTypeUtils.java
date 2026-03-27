package io.github.luminion.sqlbooster.util;

import io.github.luminion.sqlbooster.core.Booster;
import io.github.luminion.sqlbooster.core.BoosterModel;
import org.springframework.core.ResolvableType;

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
        return (Class<T>) resolveRequiredTypeArgument(booster.getClass(), Booster.class, 0, "booster entity");
    }

    @SuppressWarnings("unchecked")
    public static <T, V> Class<V> resolveBoosterVoClass(Booster<T, V> booster) {
        return (Class<V>) resolveRequiredTypeArgument(booster.getClass(), Booster.class, 1, "booster result");
    }

    @SuppressWarnings("unchecked")
    public static <V> Class<V> resolveBoosterModelResultClass(Class<?> entityClass) {
        return (Class<V>) resolveRequiredTypeArgument(entityClass, BoosterModel.class, 1, "booster model result");
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