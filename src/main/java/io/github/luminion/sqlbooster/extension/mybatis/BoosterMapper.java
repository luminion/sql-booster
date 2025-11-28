package io.github.luminion.sqlbooster.extension.mybatis;

import io.github.luminion.sqlbooster.core.BoosterSupport;
import io.github.luminion.sqlbooster.model.SqlContext;
import io.github.luminion.sqlbooster.builder.SqlBuilder;
import io.github.luminion.sqlbooster.util.SqlContextUtils;

import java.util.List;

/**
 * 一个通用的 Mapper 接口，用于执行返回视图对象 (VO) 的动态查询。
 *
 * @author luminion
 * @since 1.0.0
 */
public interface BoosterMapper<T, V> extends BoosterSupport<T, V> {

    @Override
    default List<V> selectByBooster(SqlContext<T> boosterParam, Object page) {
        SqlContext<T> sqlContext = SqlBuilder.of(this)
                .append(boosterParam)
                .build(SqlContextUtils::buildWithSuffix);
        return selectByXml(sqlContext, page);
    }


    List<V> selectByXml(SqlContext<T> sqlContext, Object page);


}
