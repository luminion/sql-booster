package io.github.luminion.sqlbooster.model;

import java.io.Serializable;
import java.util.List;

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

    <R> BPage<R> convertRecords(Class<R> clazz);
}
