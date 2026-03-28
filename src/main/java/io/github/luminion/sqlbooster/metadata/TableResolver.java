package io.github.luminion.sqlbooster.metadata;

/**
 * 表信息解析器接口。
 */
public interface TableResolver {

    /**
     * 获取解析器优先级。
     * 数字越小，优先级越高。
     */
    default int getPriority() {
        return 0;
    }

    /**
     * 解析实体类对应的表元数据。
     *
     * @param entityClass 实体类
     * @param <T> 实体类型
     * @return 表元数据，无法解析时返回 null
     */
    <T> TableMeta resolve(Class<T> entityClass);
}