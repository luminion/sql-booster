package io.github.luminion.sqlbooster.model;

import java.io.Serializable;
import java.util.List;

/**
 * 统一的分页模型接口，定义了分页对象的核心能力。
 *
 * @param <T> 记录的类型
 */
public interface BPage<T> extends Serializable {

    List<T> getRecords();

    long getTotal();

    long getCurrent();

    long getSize();

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
