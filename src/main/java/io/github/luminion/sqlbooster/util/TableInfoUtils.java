package io.github.luminion.sqlbooster.util;

import io.github.luminion.sqlbooster.core.Booster;
import io.github.luminion.sqlbooster.model.BoosterParam;
import io.github.luminion.sqlbooster.function.SFunc;
import io.github.luminion.sqlbooster.core.TableInfoProvider;
import lombok.extern.slf4j.Slf4j;

import java.util.*;
import java.util.concurrent.ConcurrentSkipListSet;

/**
 * Booster运行时核心工具类.
 * <p>
 * 提供可扩展的、针对实体和VO的功能。通过注册 {@link TableInfoProvider}，可以插入自定义逻辑.
 *
 * @author luminion
 * @since 1.0.0
 */
@Slf4j
public abstract class TableInfoUtils {
    /**
     * 已注册的 TableInfoProvider 实例.
     */
    private static final ConcurrentSkipListSet<TableInfoProvider> PROVIDERS = new ConcurrentSkipListSet<>();

    /**
     * 获取所有已注册的 Provider.
     *
     * @return Provider 列表
     * @since 1.0.0
     */
    public static List<TableInfoProvider> checkoutProviders() {
        return new ArrayList<>(PROVIDERS);
    }

    /**
     * 注册一个新的 Provider.
     *
     * @param provider 要注册的 Provider
     * @return 如果注册成功返回 true, 否则返回 false
     * @since 1.0.0
     */
    public static boolean registerProvider(TableInfoProvider provider) {
        return PROVIDERS.add(provider);
    }

    /**
     * 移除一个已注册的 Provider.
     *
     * @param provider 要移除的 Provider
     * @return 如果移除成功返回 true, 否则返回 false
     * @since 1.0.0
     */
    public static boolean removeProvider(TableInfoProvider provider) {
        return PROVIDERS.remove(provider);
    }

    /**
     * 移除指定类型的Provider.
     *
     * @param providerType 要移除的 Provider 的类型
     * @return 移除的 Provider 的数量
     * @since 1.0.0
     */
    public static int removeProvider(Class<? extends TableInfoProvider> providerType) {
        int count = 0;
        for (TableInfoProvider provider : PROVIDERS) {
            if (provider.getClass().equals(providerType)) {
                boolean remove = PROVIDERS.remove(provider);
                if (remove) {
                    count++;
                }
            }
        }
        return count;
    }


    /**
     * 将下划线命名的字符串转换为驼峰命名.
     *
     * @param str 待转换的字符串
     * @return 驼峰命名的字符串
     * @since 1.0.0
     */
    public static String underscoreToCamelCase(String str) {
        StringBuilder sb = new StringBuilder();
        boolean upperCase = false;
        for (int i = 0; i < str.length(); i++) {
            char c = str.charAt(i);
            if (c == '_') {
                upperCase = true;
            } else if (upperCase) {
                sb.append(Character.toUpperCase(c));
                upperCase = false;
            } else {
                sb.append(c);
            }
        }
        return sb.toString();
    }

    /**
     * 将驼峰命名的字符串转换为下划线命名.
     *
     * @param str 待转换的字符串
     * @return 下划线命名的字符串
     * @since 1.0.0
     */
    public static String camelCaseToUnderscore(String str) {
        StringBuilder sb = new StringBuilder();
        for (int i = 0; i < str.length(); i++) {
            char c = str.charAt(i);
            if (Character.isUpperCase(c)) {
                sb.append('_').append(Character.toLowerCase(c));
            } else {
                sb.append(c);
            }
        }
        return sb.toString();
    }

    /**
     * 获取对应的实体类的 Class 对象.
     *
     * @param boosterParam 参数
     * @param <T>          实体类型
     * @return 实体类的 Class 对象
     * @since 1.0.0
     */
    @SuppressWarnings({"unchecked"})
    public static <T, V> Class<T> getEntityClass(BoosterParam<T> boosterParam) {
        return (Class<T>) ReflectUtils.resolveTypeArguments(boosterParam.getClass(), BoosterParam.class)[0];
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
        for (TableInfoProvider provider : PROVIDERS) {
            String tableName = provider.getTableName(entityClass);
            if (tableName != null) {
                return tableName;
            }
        }
        throw new IllegalStateException("No table name found in " + PROVIDERS.size() + " providers, class: " + entityClass.getName());
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
        for (TableInfoProvider provider : PROVIDERS) {
            String idPropertyName = provider.getIdPropertyName(entityClass);
            if (idPropertyName != null) {
                return idPropertyName;
            }
        }
        throw new IllegalStateException("No IdProperty found in " + PROVIDERS.size() + " providers, class: " + entityClass.getName());
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
    public static <T, R> String getGetterPropertyName(SFunc<T, R> getter) {
        for (TableInfoProvider provider : PROVIDERS) {
            String propertyName = provider.getGetterPropertyName(getter);
            if (propertyName != null) {
                return propertyName;
            }
        }
        throw new IllegalStateException("No property name found in " + PROVIDERS.size() + " providers, getter: " + getter);
    }

    /**
     * 获取实体类的属性到数据库列别名的映射.
     *
     * @param entityClass 实体类
     * @return 属性到列别名的映射 Map
     * @since 1.0.0
     */
    public static Map<String, String> getPropertyToColumnAliasMap(Class<?> entityClass) {
        for (TableInfoProvider provider : PROVIDERS) {
            Map<String, String> contributedMap = provider.getPropertyToColumnAliasMap(entityClass);
            if (contributedMap != null && !contributedMap.isEmpty()) {
                log.debug("found alias map provider: [{}], class: [{}]", provider.getClass().getName(), entityClass.getName());
                return contributedMap;
            }
        }
        log.warn("No property to column alias map found in {} providers, class: {}", PROVIDERS.size(), entityClass.getName());
        return Collections.emptyNavigableMap();
    }
}
