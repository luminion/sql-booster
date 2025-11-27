package io.github.luminion.sqlbooster.extension.pagehelper;

import com.github.pagehelper.PageHelper;
import com.github.pagehelper.PageInfo;
import io.github.luminion.sqlbooster.core.BoosterEngine;
import io.github.luminion.sqlbooster.core.BoosterPage;
import io.github.luminion.sqlbooster.core.BoosterParam;
import io.github.luminion.sqlbooster.model.helper.SqlHelper;
import io.github.luminion.sqlbooster.model.helper.processor.SuffixProcessor;

/**
 * 针对 PageHelper 的 BoosterEngine 扩展接口.
 * <p>
 * 集成了 {@link BoosterEngine} 的能力, 提供基于 PageHelper 的分页查询功能.
 *
 * @param <T> 实体类型
 * @param <V> VO 类型
 * @author luminion
 * @since 1.0.0
 */
public interface PageHelperBooster<T, V> extends BoosterEngine<T, V> {

    @Override
    default BoosterPage<V> voPage(BoosterParam<T> boosterParam, long pageNum, long pageSize) {
        PageInfo<V> pageInfo = PageHelper.startPage((int) pageNum, (int) pageSize)
                .doSelectPageInfo(() -> selectByBooster(boosterParam, null));
        PageHelperPage<V> page = new PageHelperPage<>(pageInfo);
        return page;
    }
}