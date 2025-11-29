package io.github.luminion.sqlbooster.model;

import java.io.Serializable;
import java.util.List;

/**
 * 统一的分页模型接口，定义了分页对象的核心能力。
 *
 * @param <T> 记录的类型
 */
public interface BPage<T> extends Serializable {

    /**
     * 获取当前页的记录列表。
     */
    List<T> getRecords();

    /**
     * 获取总记录数。
     */
    long getTotal();

    /**
     * 获取当前页码。
     */
    long getCurrent();

    /**
     * 获取每页显示的条数。
     */
    long getSize();

    /**
     * 获取总页数。
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
     * 将分页结果中的记录列表转换为另一种类型。
     *
     * @param clazz 目标类型
     * @return 包含新类型记录的分页对象
     */
    <R> BPage<R> convertRecords(Class<R> clazz);
}
