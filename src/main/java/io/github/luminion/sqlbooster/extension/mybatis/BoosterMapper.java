package io.github.luminion.sqlbooster.extension.mybatis;

import io.github.luminion.sqlbooster.core.BoosterSupport;
import io.github.luminion.sqlbooster.model.SqlContext;
import io.github.luminion.sqlbooster.util.SqlContextUtils;

import java.util.List;

/**
 * 原生 MyBatis 场景的入口。
 * 它负责把 `SqlContext` 规范化后交给 XML；如果要分页，需要具体实现自己接入分页插件。
 */
public interface BoosterMapper<T, V> extends BoosterSupport<T, V> {

    @Override
    default List<V> doFetch(SqlContext<T> sqlContext, Object page) {
        SqlContext<T> normalize = SqlContextUtils.normalize(boosterEntityClass(), sqlContext);
        return selectByXml(normalize, page);
    }

    List<V> selectByXml(SqlContext<T> sqlContext, Object page);
}
