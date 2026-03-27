package io.github.luminion.sqlbooster.metadata;

import io.github.luminion.sqlbooster.core.Booster;

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

/**
 * Booster 默认实现注册表。
 */
public abstract class BoosterRegistry {

    private static final Map<Class<?>, Registration<?>> DEFAULT_BOOSTERS = new ConcurrentHashMap<>();

    public static void clear() {
        DEFAULT_BOOSTERS.clear();
    }

    public static <T, V> void registerBooster(Booster<T, V> booster) {
        registerBooster(null, booster);
    }

    public static <T, V> void registerBooster(String sourceName, Booster<T, V> booster) {
        Registration<T> registration = new Registration<>(booster.boosterEntityClass(), booster.boosterResultClass(), booster,
                sourceName);
        Registration<?> existing = DEFAULT_BOOSTERS.putIfAbsent(registration.entityClass, registration);
        if (existing != null && existing.booster != booster) {
            throw new IllegalStateException("Multiple default boosters found for entity "
                    + registration.entityClass.getName() + ": " + existing.describe() + " and "
                    + registration.describe());
        }
    }

    @SuppressWarnings("unchecked")
    public static <T, V> Booster<T, V> getRequiredBooster(Class<T> entityClass, Class<V> resultClass) {
        Registration<?> registration = DEFAULT_BOOSTERS.get(entityClass);
        if (registration == null) {
            throw new IllegalStateException("No default booster registered for entity " + entityClass.getName());
        }
        if (registration.resultClass.equals(resultClass)) {
            return (Booster<T, V>) registration.booster;
        }
        throw new IllegalStateException("No default booster registered for entity " + entityClass.getName()
                + " and result type " + resultClass.getName() + ", registered result type is "
                + registration.resultClass.getName());
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