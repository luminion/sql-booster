package io.github.luminion.sqlbooster.core;

import io.github.luminion.sqlbooster.function.SFunc;
import lombok.extern.slf4j.Slf4j;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

import java.util.concurrent.CopyOnWriteArrayList;

/**
 * Booster 运行时核心元数据注册表。
 * <p>
 * 通过注册 {@link TableResolver} 扩展，为实体和VO提供元信息解析能力。
 * <p>
 */
@Slf4j
public abstract class TableMetaRegistry {

    private static final CopyOnWriteArrayList<TableResolver> RESOLVERS = new CopyOnWriteArrayList<>();
    /**
     * 缓存：实体类 -> 表名
     */
    private static final Map<Class<?>, String> TABLE_NAME_CACHE = new ConcurrentHashMap<>();
    /**
     * 缓存：实体类 -> 主键属性名
     */
    private static final Map<Class<?>, String> ID_PROPERTY_CACHE = new ConcurrentHashMap<>();
    /**
     * 缓存：实体类 -> 字段映射
     */
    private static final Map<Class<?>, Map<String, String>> ALIAS_MAP_CACHE = new ConcurrentHashMap<>();

    public static List<TableResolver> checkoutTableResolver() {
        return new ArrayList<>(RESOLVERS);
    }

    public static boolean addTableResolver(TableResolver tableResolver) {
        boolean added = RESOLVERS.add(tableResolver);
        RESOLVERS.sort(TableResolver::compareTo);
        if (added) {
            clearCache();
        }
        return added;
    }

    public static boolean removeTableResolver(TableResolver tableResolver) {
        boolean removed = RESOLVERS.remove(tableResolver);
        if (removed) {
            clearCache();
        }
        return removed;
    }

    public static int removeTableResolver(Class<? extends TableResolver> tableResolverType) {
        int count = 0;
        for (TableResolver tableResolver : RESOLVERS) {
            if (tableResolver.getClass().equals(tableResolverType)) {
                boolean remove = RESOLVERS.remove(tableResolver);
                if (remove) {
                    count++;
                }
            }
        }
        if (count > 0) {
            clearCache();
        }
        return count;
    }

    /**
     * 清空所有元数据缓存。
     * 当 Resolver 发生变更时调用，防止缓存脏读。
     */
    private static void clearCache() {
        TABLE_NAME_CACHE.clear();
        ID_PROPERTY_CACHE.clear();
        ALIAS_MAP_CACHE.clear();
        log.debug("TableMetaRegistry cache cleared.");
    }

    /**
     * 获取实体类对应的数据库表名。
     *
     * @param entityClass 实体类
     * @return 表名
     * @throws IllegalStateException 如果没有找到对应的表名
     */
    public static String getTableName(Class<?> entityClass) {
        return TABLE_NAME_CACHE.computeIfAbsent(entityClass, key -> {
            for (TableResolver tableResolver : RESOLVERS) {
                String tableName = tableResolver.getTableName(key);
                if (tableName != null) {
                    return tableName;
                }
            }
            throw new IllegalStateException(
                    "No table name found in " + RESOLVERS.size() + " tableResolvers, class: " + key.getName());
        });
    }

    /**
     * 获取实体类的主键属性名。
     *
     * @param entityClass 实体类
     * @return 主键属性名
     * @throws IllegalStateException 如果没有找到主键属性
     */
    public static String getIdPropertyName(Class<?> entityClass) {
        return ID_PROPERTY_CACHE.computeIfAbsent(entityClass, key -> {
            for (TableResolver tableResolver : RESOLVERS) {
                String idPropertyName = tableResolver.getIdPropertyName(key);
                if (idPropertyName != null) {
                    return idPropertyName;
                }
            }
            throw new IllegalStateException(
                    "No IdProperty found in " + RESOLVERS.size() + " tableResolvers, class: " + key.getName());
        });
    }

    /**
     * 从 getter 方法引用中获取属性名。
     * <p>
     *
     * @param getter getter 方法引用
     * @return 属性名
     * @throws IllegalStateException 如果没有找到对应的属性名
     */
    public static <T, R> String getGetterPropertyName(SFunc<T, R> getter) {
        for (TableResolver tableResolver : RESOLVERS) {
            String propertyName = tableResolver.getGetterPropertyName(getter);
            if (propertyName != null) {
                return propertyName;
            }
        }
        throw new IllegalStateException(
                "No property name found in " + RESOLVERS.size() + " tableResolvers, getter: " + getter);
    }

    /**
     * 获取实体类的属性到数据库列别名的映射。
     *
     * @param entityClass 实体类
     * @return 属性到列别名的映射 Map
     */
    public static Map<String, String> getPropertyToColumnAliasMap(Class<?> entityClass) {
        return ALIAS_MAP_CACHE.computeIfAbsent(entityClass, key -> {
            for (TableResolver tableResolver : RESOLVERS) {
                Map<String, String> contributedMap = tableResolver.getPropertyToColumnAliasMap(key);
                if (contributedMap != null && !contributedMap.isEmpty()) {
                    log.debug("found alias map tableResolver: [{}], class: [{}]", tableResolver.getClass().getName(),
                            key.getName());
                    return contributedMap;
                }
            }
            log.warn("No property to column alias map found in {} tableResolvers, class: {}", RESOLVERS.size(),
                    key.getName());
            return Collections.emptyMap();
        });
    }
}