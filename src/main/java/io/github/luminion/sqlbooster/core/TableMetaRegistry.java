package io.github.luminion.sqlbooster.core;

import io.github.luminion.sqlbooster.model.SqlContext;
import io.github.luminion.sqlbooster.function.GetterReference;
import io.github.luminion.sqlbooster.util.ReflectUtils;
import lombok.extern.slf4j.Slf4j;

import java.util.*;
import java.util.concurrent.ConcurrentSkipListSet;

/**
 * Booster运行时核心工具类.
 * <p>
 * 提供可扩展的、针对实体和VO的功能。通过注册 {@link TableResolver}，可以插入自定义逻辑.
 *
 * @author luminion
 * @since 1.0.0
 */
@Slf4j
public abstract class TableMetaRegistry {
    /**
     * 已注册的 TableResolver 实例.
     */
    private static final ConcurrentSkipListSet<TableResolver> RESOLVERS = new ConcurrentSkipListSet<>();

    /**
     * 获取所有已注册的 Provider.
     *
     * @return Provider 列表
     * @since 1.0.0
     */
    public static List<TableResolver> checkoutProviders() {
        return new ArrayList<>(RESOLVERS);
    }

    /**
     * 注册一个新的 Provider.
     *
     * @param tableResolver 要注册的 Provider
     * @return 如果注册成功返回 true, 否则返回 false
     * @since 1.0.0
     */
    public static boolean registerProvider(TableResolver tableResolver) {
        return RESOLVERS.add(tableResolver);
    }

    /**
     * 移除一个已注册的 Provider.
     *
     * @param tableResolver 要移除的
     * @return 如果移除成功返回 true, 否则返回 false
     * @since 1.0.0
     */
    public static boolean removeProvider(TableResolver tableResolver) {
        return RESOLVERS.remove(tableResolver);
    }

    /**
     * 移除指定类型的Provider.
     *
     * @param tableResolverType 要移除的的类型(严格匹配, 不包含子类)
     * @return 移除的数量
     * @since 1.0.0
     */
    public static int removeProvider(Class<? extends TableResolver> tableResolverType) {
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
     * 获取对应的实体类的 Class 对象.
     *
     * @param sqlContext 参数
     * @param <T>          实体类型
     * @return 实体类的 Class 对象
     * @since 1.0.0
     */
    @SuppressWarnings({"unchecked"})
    public static <T, V> Class<T> getEntityClass(SqlContext<T> sqlContext) {
        return (Class<T>) ReflectUtils.resolveTypeArguments(sqlContext.getClass(), SqlContext.class)[0];
    }

    /**
     * 获取对应的实体类的 Class 对象.
     *
     * @param booster Booster 实例
     * @param <T>     实体类型
     * @param <V>     VO 类型
     * @return 实体类的 Class 对象
     * @since 1.0.0
     */
    @SuppressWarnings({"unchecked"})
    public static <T, V> Class<T> getEntityClass(Booster<T, V> booster) {
        return (Class<T>) ReflectUtils.resolveTypeArguments(booster.getClass(), Booster.class)[0];
    }

    /**
     * 从 Booster 实现类中获取 VO 类的 Class 对象.
     *
     * @param booster Booster 实例
     * @param <T>     实体类型
     * @param <V>     VO 类型
     * @return VO 类的 Class 对象
     * @since 1.0.0
     */
    @SuppressWarnings({"unchecked"})
    public static <T, V> Class<V> getViewObjectClass(Booster<T, V> booster) {
        return (Class<V>) ReflectUtils.resolveTypeArguments(booster.getClass(), Booster.class)[1];
    }

    /**
     * 获取实体类对应的数据库表名.
     *
     * @param entityClass 实体类
     * @return 表名
     * @throws IllegalStateException 如果没有找到对应的表名
     * @since 1.0.0
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
     * 获取实体类的主键属性名.
     *
     * @param entityClass 实体类
     * @return 主键属性名
     * @throws IllegalStateException 如果没有找到主键属性
     * @since 1.0.0
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
     * 从 getter 方法引用中获取属性名.
     *
     * @param getter getter 方法引用
     * @param <T>    实体类型
     * @param <R>    属性类型
     * @return 属性名
     * @throws IllegalStateException 如果没有找到对应的属性名
     * @since 1.0.0
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
     * 获取实体类的属性到数据库列别名的映射.
     *
     * @param entityClass 实体类
     * @return 属性到列别名的映射 Map
     * @since 1.0.0
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
        return Collections.emptyNavigableMap();
    }
}
