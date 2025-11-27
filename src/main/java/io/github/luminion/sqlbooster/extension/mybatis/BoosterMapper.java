package io.github.luminion.sqlbooster.extension.mybatis;

import io.github.luminion.sqlbooster.core.BoosterEngine;
import io.github.luminion.sqlbooster.core.BoosterParam;
import io.github.luminion.sqlbooster.model.api.QueryParam;
import io.github.luminion.sqlbooster.model.helper.SqlHelper;
import io.github.luminion.sqlbooster.model.helper.processor.SuffixProcessor;
import io.github.luminion.sqlbooster.util.BoostUtils;
import org.apache.ibatis.annotations.Param;

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
        Class<T> entityClass = BoostUtils.getEntityClass(this);
        SqlHelper<T> sqlHelper = SqlHelper.of(entityClass)
                .append(boosterParam)
                .process(SuffixProcessor.of()::process);
        return selectByXml(sqlHelper, page);
    }


    List<V> selectByXml(QueryParam<T> queryParam, Object page);


}
