package io.github.luminion.sqlbooster.core;

import io.github.luminion.sqlbooster.function.SFunc;

import java.util.Map;

/**
 * 表信息解析器接口。
 * <p>
 * 定义了如何从实体类中获取数据库元信息（如表名、主键、字段映射等）的规范。
 * 如果未找到对应的元信息，将返回 null。
 * 
 */
public interface TableResolver extends Comparable<TableResolver> {
    /**
     * 1. 优先比较{@link #getPriority()},越小的优先级越高
     * 2. 比类名字符串的自然顺序
     * 3. 内存地址（hashCode）的比较，确保不同对象的优先级不同
     */
    @Override
    default int compareTo(TableResolver o) {
        // 1. 第一轮：比较优先级
        // 如果你要 "数字越小优先级越高" (Smallest First)，应该是 this - o
        // 如果你要 "数字越大优先级越高" (Largest First)，应该是 o - this
        // 这里我按照你的注释改为：数字越小越靠前
        int priorityCompare = Integer.compare(this.getPriority(), o.getPriority());

        if (priorityCompare != 0) {
            return priorityCompare;
        }

        // 2. 第二轮：优先级相同，比较类名字符串
        // 这样保证不同实现的类（即使优先级一样）都能注册进去
        int nameCompare = this.getClass().getName().compareTo(o.getClass().getName());
        if (nameCompare != 0) {
            return nameCompare;
        }

        // 3. 第三轮：类名也相同（同一个类的两个实例），比较内存地址/HashCode
        // 只有完全同一个对象才返回 0
        return Integer.compare(System.identityHashCode(this), System.identityHashCode(o));
    }

    /**
     * 获取当前解析器的优先级。
     * 数字越小，优先级越高 (排在 List 的前面，先被执行)。
     */
    int getPriority();

    /**
     * 根据实体类获取表名。
     */
    <T> String getTableName(Class<T> entityClass);

    /**
     * 根据实体类获取主键属性名。
     */
    <T> String getIdPropertyName(Class<T> entityClass);

    /**
     * 从 getter 方法引用中获取属性名。
     */
    <T, R> String getGetterPropertyName(SFunc<T, R> getter);
    
    /**
     * 获取实体类的属性到数据库列名(或别名)的映射。
     */
    <T> Map<String, String> getPropertyToColumnAliasMap(Class<T> entityClass);
}
