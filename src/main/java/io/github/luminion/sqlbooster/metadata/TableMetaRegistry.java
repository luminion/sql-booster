package io.github.luminion.sqlbooster.metadata;

import io.github.luminion.sqlbooster.function.SFunc;
import io.github.luminion.sqlbooster.util.LambdaUtils;
import lombok.extern.slf4j.Slf4j;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Comparator;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.CopyOnWriteArrayList;

@Slf4j
public abstract class TableMetaRegistry {

    private static final Comparator<TableResolver> RESOLVER_COMPARATOR = (left, right) -> {
        // 先按显式优先级，再按类名兜底，保证不同 JVM 里解析顺序也稳定。
        int priorityCompare = Integer.compare(left.getPriority(), right.getPriority());
        if (priorityCompare != 0) {
            return priorityCompare;
        }
        int nameCompare = left.getClass().getName().compareTo(right.getClass().getName());
        if (nameCompare != 0) {
            return nameCompare;
        }
        return Integer.compare(System.identityHashCode(left), System.identityHashCode(right));
    };

    private static final CopyOnWriteArrayList<TableResolver> RESOLVERS = new CopyOnWriteArrayList<>();
    private static final Map<Class<?>, TableMeta> TABLE_META_CACHE = new ConcurrentHashMap<>();

    public static List<TableResolver> checkoutTableResolver() {
        return new ArrayList<>(RESOLVERS);
    }

    public static boolean addTableResolver(TableResolver tableResolver) {
        boolean added = RESOLVERS.add(tableResolver);
        if (added) {
            RESOLVERS.sort(RESOLVER_COMPARATOR);
            // 解析器集合变了，旧缓存可能已经失效，必须整批清掉。
            clearCache();
        }
        return added;
    }

    public static int addTableResolvers(Collection<? extends TableResolver> tableResolvers) {
        if (tableResolvers == null || tableResolvers.isEmpty()) {
            return 0;
        }
        int before = RESOLVERS.size();
        boolean changed = RESOLVERS.addAll(tableResolvers);
        if (changed) {
            RESOLVERS.sort(RESOLVER_COMPARATOR);
            clearCache();
        }
        return RESOLVERS.size() - before;
    }

    public static int addTableResolvers(TableResolver... tableResolvers) {
        if (tableResolvers == null || tableResolvers.length == 0) {
            return 0;
        }
        return addTableResolvers(Arrays.asList(tableResolvers));
    }

    public static boolean removeTableResolver(TableResolver tableResolver) {
        boolean removed = RESOLVERS.remove(tableResolver);
        if (removed) {
            clearCache();
        }
        return removed;
    }

    public static int removeTableResolver(Class<? extends TableResolver> tableResolverType) {
        int before = RESOLVERS.size();
        boolean removed = RESOLVERS.removeIf(tableResolver -> tableResolver.getClass().equals(tableResolverType));
        if (removed) {
            clearCache();
        }
        return before - RESOLVERS.size();
    }

    private static void clearCache() {
        TABLE_META_CACHE.clear();
        log.debug("TableMetaRegistry cache cleared.");
    }

    public static String getTableName(Class<?> entityClass) {
        TableMeta tableMeta = getTableMeta(entityClass);
        if (tableMeta.getTableName() != null) {
            return tableMeta.getTableName();
        }
        throw new IllegalStateException(
                "No table name found in " + RESOLVERS.size() + " tableResolvers, class: " + entityClass.getName());
    }

    public static String getIdPropertyName(Class<?> entityClass) {
        TableMeta tableMeta = getTableMeta(entityClass);
        if (tableMeta.getIdPropertyName() != null) {
            return tableMeta.getIdPropertyName();
        }
        throw new IllegalStateException(
                "No IdProperty found in " + RESOLVERS.size() + " tableResolvers, class: " + entityClass.getName());
    }

    public static <T, R> String getGetterPropertyName(SFunc<T, R> getter) {
        return LambdaUtils.resolveGetterPropertyName(getter);
    }

    public static Map<String, String> getPropertyToColumnAliasMap(Class<?> entityClass) {
        return getTableMeta(entityClass).getPropertyToColumnAliasMap();
    }

    private static TableMeta getTableMeta(Class<?> entityClass) {
        TableMeta cached = TABLE_META_CACHE.get(entityClass);
        if (cached != null) {
            return cached;
        }
        for (TableResolver tableResolver : RESOLVERS) {
            // 命中第一个可解析的 resolver 就返回，后面的低优先级 resolver 不再参与。
            TableMeta tableMeta = tableResolver.resolve(entityClass);
            if (tableMeta != null) {
                TableMeta existing = TABLE_META_CACHE.putIfAbsent(entityClass, tableMeta);
                return existing != null ? existing : tableMeta;
            }
        }
        // 无人解析出结果时返回全空兜底，但不写缓存：resolver 可能尚未初始化完成（如 MP 的 TableInfo 懒加载），
        // 若缓存空 meta，之后即使就绪也会一直命中空值而持续抛错。
        return new TableMeta(null, null, null);
    }
}
