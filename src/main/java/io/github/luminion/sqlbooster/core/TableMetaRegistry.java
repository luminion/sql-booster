package io.github.luminion.sqlbooster.core;

import io.github.luminion.sqlbooster.function.GetterReference;
import lombok.extern.slf4j.Slf4j;

import java.util.Collections;
import java.util.Map;

/**
 * Booster 运行时核心元数据注册表。
 * <p>
 * 为实体和 VO 提供元信息解析能力。
 */
@Slf4j
public abstract class TableMetaRegistry {
    /**
     * 全局唯一的解析器实例。
     * 默认为 {@link DefaultTableResolver}，策略为驼峰转下划线。
     */
    private static TableResolver tableResolver = new DefaultTableResolver(true);

    /**
     * 注册（或替换）全局的元数据解析器。
     * <p>
     * 如果主要从 Spring 上下文初始化，可在此调用。
     *
     * @param resolver 新的解析器实例
     */
    public static void register(TableResolver resolver) {
        if (resolver == null) {
            throw new IllegalArgumentException("resolver can not be null");
        }
        log.info("TableMetaRegistry switch resolver to: [{}]", resolver.getClass().getName());
        tableResolver = resolver;
    }

    /**
     * 获取实体类对应的数据库表名。
     *
     * @param entityClass 实体类
     * @return 表名
     */
    public static String getTableName(Class<?> entityClass) {
        String tableName = tableResolver.getTableName(entityClass);
        if (tableName == null) {
            throw new IllegalStateException("No table name found for class: " + entityClass.getName());
        }
        return tableName;
    }

    /**
     * 获取实体类的主键属性名。
     *
     * @param entityClass 实体类
     * @return 主键属性名
     */
    public static String getIdPropertyName(Class<?> entityClass) {
        String idPropertyName = tableResolver.getIdPropertyName(entityClass);
        if (idPropertyName == null) {
            throw new IllegalStateException("No IdProperty found for class: " + entityClass.getName());
        }
        return idPropertyName;
    }

    /**
     * 从 getter 方法引用中获取属性名。
     *
     * @param getter getter 方法引用
     * @return 属性名
     */
    public static <T, R> String getGetterPropertyName(GetterReference<T, R> getter) {
        String propertyName = tableResolver.getGetterPropertyName(getter);
        if (propertyName == null) {
            throw new IllegalStateException("No property name found for getter: " + getter);
        }
        return propertyName;
    }

    /**
     * 获取实体类的属性到数据库列别名的映射。
     *
     * @param entityClass 实体类
     * @return 属性到列别名的映射 Map
     */
    public static Map<String, String> getPropertyToColumnAliasMap(Class<?> entityClass) {
        Map<String, String> map = tableResolver.getPropertyToColumnAliasMap(entityClass);
        if (map == null) {
            return Collections.emptyMap();
        }
        return map;
    }
}
