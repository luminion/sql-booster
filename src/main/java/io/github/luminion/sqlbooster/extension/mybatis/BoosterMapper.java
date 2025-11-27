package io.github.luminion.sqlbooster.extension.mybatis;

import io.github.luminion.sqlbooster.core.BoosterEngine;
import io.github.luminion.sqlbooster.core.BoosterParam;
import io.github.luminion.sqlbooster.model.api.SqlContext;
import io.github.luminion.sqlbooster.model.builder.SqlBuilder;
import io.github.luminion.sqlbooster.util.BuildUtils;

import java.util.List;

/**
 * 一个通用的 Mapper 接口，用于执行返回视图对象 (VO) 的动态查询。
 *
 * @author luminion
 * @since 1.0.0
 */
public interface BoosterMapper<T, V> extends BoosterEngine<T, V> {

    @Override
    default List<V> selectByBooster(BoosterParam<T> boosterParam, Object page) {
        SqlBuilder<T> sqlBuilder = SqlBuilder.of(this)
                .append(boosterParam)
                .build(BuildUtils::buildWithSuffix);
        return selectByXml(sqlBuilder, page);
    }


    List<V> selectByXml(SqlContext<T> sqlContext, Object page);


}
