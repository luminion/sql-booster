package io.github.luminion.sqlbooster.model;

import java.io.Serializable;
import java.util.List;

public interface BPage<T> extends Serializable {

    List<T> getRecords();

    long getTotal();

    long getCurrent();

    long getSize();

    default long getPages() {
        // size 非正时无法计算页数（负 size 在 MP 里表示不分页），直接返回 0，避免 total / 负数 得到负页数。
        if (getSize() <= 0) {
            return 0L;
        }
        long pages = getTotal() / getSize();
        if (getTotal() % getSize() != 0) {
            pages++;
        }
        return pages;
    }

    <R> BPage<R> convertRecords(Class<R> clazz);
}
