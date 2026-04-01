package io.github.luminion.sqlbooster.metadata;

import io.github.luminion.sqlbooster.core.Booster;

import java.util.Map;
import java.util.Objects;
import java.util.concurrent.ConcurrentHashMap;
import java.util.stream.Collectors;

public abstract class BoosterRegistry {

    // 同一实体可以挂多个结果类型，所以 key 不能只看 entityClass。
    private static final Map<RegistrationKey, Registration<?>> DEFAULT_BOOSTERS = new ConcurrentHashMap<>();

    public static void clear() {
        DEFAULT_BOOSTERS.clear();
    }

    public static <T, V> void registerBooster(Booster<T, V> booster) {
        registerBooster(null, booster);
    }

    public static <T, V> void registerBooster(String sourceName, Booster<T, V> booster) {
        Registration<T> registration = new Registration<>(booster.boosterEntityClass(), booster.boosterResultClass(), booster,
                sourceName);
        RegistrationKey key = new RegistrationKey(registration.entityClass, registration.resultClass);
        Registration<?> existing = DEFAULT_BOOSTERS.putIfAbsent(key, registration);
        if (existing != null && existing.booster != booster) {
            throw new IllegalStateException("Multiple default boosters found for entity "
                    + registration.entityClass.getName() + " and result type " + registration.resultClass.getName()
                    + ": " + existing.describe() + " and "
                    + registration.describe());
        }
    }

    public static boolean removeBooster(Booster<?, ?> booster) {
        if (booster == null) {
            return false;
        }
        return DEFAULT_BOOSTERS.entrySet().removeIf(entry -> entry.getValue().booster == booster);
    }

    @SuppressWarnings("unchecked")
    public static <T, V> Booster<T, V> getRequiredBooster(Class<T> entityClass, Class<V> resultClass) {
        Registration<?> registration = DEFAULT_BOOSTERS.get(new RegistrationKey(entityClass, resultClass));
        if (registration != null) {
            return (Booster<T, V>) registration.booster;
        }
        // 找不到精确结果类型时，把当前实体下已有的结果类型列出来，方便定位注册冲突或漏注册。
        String availableResultTypes = DEFAULT_BOOSTERS.keySet().stream()
                .filter(key -> key.entityClass.equals(entityClass))
                .map(key -> key.resultClass.getName())
                .sorted()
                .collect(Collectors.joining(", "));
        if (availableResultTypes.isEmpty()) {
            throw new IllegalStateException("No default booster registered for entity " + entityClass.getName());
        }
        throw new IllegalStateException("No default booster registered for entity " + entityClass.getName()
                + " and result type " + resultClass.getName() + ", available result types are: "
                + availableResultTypes);
    }

    private static final class RegistrationKey {
        private final Class<?> entityClass;
        private final Class<?> resultClass;

        private RegistrationKey(Class<?> entityClass, Class<?> resultClass) {
            this.entityClass = entityClass;
            this.resultClass = resultClass;
        }

        @Override
        public boolean equals(Object o) {
            if (this == o) {
                return true;
            }
            if (!(o instanceof RegistrationKey)) {
                return false;
            }
            RegistrationKey that = (RegistrationKey) o;
            return Objects.equals(entityClass, that.entityClass) && Objects.equals(resultClass, that.resultClass);
        }

        @Override
        public int hashCode() {
            return Objects.hash(entityClass, resultClass);
        }
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
