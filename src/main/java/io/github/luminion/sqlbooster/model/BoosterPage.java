package io.github.luminion.sqlbooster.model;

import java.util.List;

/**
 * 统一分页接口,定义了分页对象的核心能力
 * @param <T> 记录的类型
 * @author luminion
 * @since 1.0.0
 */
public interface BoosterPage<T> {
    /**
     * 获取记录列表
     *
     * @return 记录列表
     * @since 1.0.0
     */
    List<T> getRecords();

    /**
     * 获取总记录数
     *
     * @return 总记录数
     * @since 1.0.0
     */
    long getTotal();

    /**
     * 获取当前页码
     *
     * @return 当前页码
     * @since 1.0.0
     */
    long getCurrent();

    /**
     * 获取每页数量
     *
     * @return 每页数量
     * @since 1.0.0
     */
    long getSize();

    /**
     * 获取总页数
     *
     * @return 总页数
     * @since 1.0.0
     */
    default long getPages() {
        if (getSize() == 0) {
            return 0L;
        }
        long pages = getTotal() / getSize();
        if (getTotal() % getSize() != 0) {
            pages++;
        }
        return pages;
    }
    
    /**
     * 转换记录列表的类型
     *
     * @param clazz 目标类型
     * @return 转换后的记录列表
     * @since 1.0.0
     */
    <R> BoosterPage<R> convertRecords(Class<R> clazz);
}
