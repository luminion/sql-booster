package io.github.luminion.sqlbooster.core;

import io.github.luminion.sqlbooster.function.GetterReference;
import lombok.extern.slf4j.Slf4j;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentSkipListSet;

/**
 * Booster 运行时核心元数据注册表。
 * <p>
 * 通过注册 {@link TableResolver} 扩展，为实体和VO提供元信息解析能力。
 */
@Slf4j
public abstract class TableMetaRegistry {

    private static final ConcurrentSkipListSet<TableResolver> RESOLVERS = new ConcurrentSkipListSet<>();

    public static List<TableResolver> checkoutTableResolver() {
        return new ArrayList<>(RESOLVERS);
    }

    public static boolean addTableResolver(TableResolver tableResolver) {
        return RESOLVERS.add(tableResolver);
    }

    public static boolean removeTableResolver(TableResolver tableResolver) {
        return RESOLVERS.remove(tableResolver);
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
        return count;
    }

    /**
     * 获取实体类对应的数据库表名。
     *
     * @param entityClass 实体类
     * @return 表名
     * @throws IllegalStateException 如果没有找到对应的表名
     */
    public static String getTableName(Class<?> entityClass) {
        for (TableResolver tableResolver : RESOLVERS) {
            String tableName = tableResolver.getTableName(entityClass);
            if (tableName != null) {
                return tableName;
            }
        }
        throw new IllegalStateException("No table name found in " + RESOLVERS.size() + " tableResolvers, class: " + entityClass.getName());
    }

    /**
     * 获取实体类的主键属性名。
     *
     * @param entityClass 实体类
     * @return 主键属性名
     * @throws IllegalStateException 如果没有找到主键属性
     */
    public static String getIdPropertyName(Class<?> entityClass) {
        for (TableResolver tableResolver : RESOLVERS) {
            String idPropertyName = tableResolver.getIdPropertyName(entityClass);
            if (idPropertyName != null) {
                return idPropertyName;
            }
        }
        throw new IllegalStateException("No IdProperty found in " + RESOLVERS.size() + " tableResolvers, class: " + entityClass.getName());
    }

    /**
     * 从 getter 方法引用中获取属性名。
     *
     * @param getter getter 方法引用
     * @return 属性名
     * @throws IllegalStateException 如果没有找到对应的属性名
     */
    public static <T, R> String getGetterPropertyName(GetterReference<T, R> getter) {
        for (TableResolver tableResolver : RESOLVERS) {
            String propertyName = tableResolver.getGetterPropertyName(getter);
            if (propertyName != null) {
                return propertyName;
            }
        }
        throw new IllegalStateException("No property name found in " + RESOLVERS.size() + " tableResolvers, getter: " + getter);
    }

    /**
     * 获取实体类的属性到数据库列别名的映射。
     *
     * @param entityClass 实体类
     * @return 属性到列别名的映射 Map
     */
    public static Map<String, String> getPropertyToColumnAliasMap(Class<?> entityClass) {
        for (TableResolver tableResolver : RESOLVERS) {
            Map<String, String> contributedMap = tableResolver.getPropertyToColumnAliasMap(entityClass);
            if (contributedMap != null && !contributedMap.isEmpty()) {
                log.debug("found alias map tableResolver: [{}], class: [{}]", tableResolver.getClass().getName(), entityClass.getName());
                return contributedMap;
            }
        }
        log.warn("No property to column alias map found in {} tableResolvers, class: {}", RESOLVERS.size(), entityClass.getName());
        return Collections.emptyMap();
    }
}