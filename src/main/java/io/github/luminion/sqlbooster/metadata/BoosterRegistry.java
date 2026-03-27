package io.github.luminion.sqlbooster.metadata;

import io.github.luminion.sqlbooster.core.LambdaBooster;
import io.github.luminion.sqlbooster.core.Booster;

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

/**
 * Registry for default boosters.
 */
public abstract class BoosterRegistry {

    private static final Map<Class<?>, Registration<?>> DEFAULT_BOOSTERS = new ConcurrentHashMap<>();

    public static void clear() {
        DEFAULT_BOOSTERS.clear();
    }

    public static <T, V> void registerDefault(Booster<T, V> booster) {
        registerDefault(null, booster);
    }

    public static <T, V> void registerDefault(String sourceName, Booster<T, V> booster) {
        Registration<T> registration = new Registration<>(booster.entityClass(), booster.resultClass(), booster,
                sourceName);
        Registration<?> existing = DEFAULT_BOOSTERS.putIfAbsent(registration.entityClass, registration);
        if (existing != null && existing.booster != booster) {
            throw new IllegalStateException("Multiple default boosters found for entity "
                    + registration.entityClass.getName() + ": " + existing.describe() + " and "
                    + registration.describe());
        }
    }

    @SuppressWarnings("unchecked")
    public static <T, V> Booster<T, V> getRequiredDefault(Class<T> entityClass) {
        return (Booster<T, V>) getRequiredRegistration(entityClass).booster;
    }

    @SuppressWarnings("unchecked")
    public static <T, V> Booster<T, V> getRequired(Class<T> entityClass, Class<V> targetType) {
        Registration<T> registration = getRequiredRegistration(entityClass);
        if (registration.resultClass.equals(targetType)) {
            return (Booster<T, V>) registration.booster;
        }
        throw new IllegalStateException("No default booster registered for entity " + entityClass.getName()
                + " and result type " + targetType.getName() + ", registered result type is "
                + registration.resultClass.getName());
    }

    public static <T, V> LambdaBooster<T, V> lambda(Class<T> entityClass, Class<V> targetType) {
        return getRequired(entityClass, targetType).lambdaBooster();
    }

    @SuppressWarnings("unchecked")
    private static <T> Registration<T> getRequiredRegistration(Class<T> entityClass) {
        Registration<?> registration = DEFAULT_BOOSTERS.get(entityClass);
        if (registration == null) {
            throw new IllegalStateException("No default booster registered for entity " + entityClass.getName());
        }
        return (Registration<T>) registration;
    }

    private static final class Registration<T> {
        private final Class<T> entityClass;
        private final Class<?> resultClass;
        private final Booster<T, ?> booster;
        private final String sourceName;

        private Registration(Class<T> entityClass, Class<?> resultClass, Booster<T, ?> booster, String sourceName) {
            this.entityClass = entityClass;
            this.resultClass = resultClass;
            this.booster = booster;
            this.sourceName = sourceName;
        }

        private String describe() {
            return sourceName == null ? booster.getClass().getName() : sourceName;
        }
    }
}