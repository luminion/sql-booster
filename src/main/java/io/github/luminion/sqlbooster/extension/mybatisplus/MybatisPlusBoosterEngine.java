package io.github.luminion.sqlbooster.extension.mybatisplus;

import com.baomidou.mybatisplus.extension.plugins.pagination.PageDTO;
import io.github.luminion.sqlbooster.core.BoosterEngine;
import io.github.luminion.sqlbooster.core.BoosterPage;
import io.github.luminion.sqlbooster.core.BoosterParam;

import java.util.List;

/**
 * @author luminion
 * @since 1.0.0
 */
public interface MybatisPlusBoosterEngine<T, V> extends BoosterEngine<T, V> {
    @Override
    default BoosterPage<V> voPage(BoosterParam<T> boosterParam, long pageNum, long pageSize) {
        PageDTO<V> pageInfo = new PageDTO<>(pageNum, pageSize);
        List<V> vs = selectByBooster(boosterParam, pageInfo);
        pageInfo.setRecords(vs);
        return new MybatisPlusBoosterPage<>(pageInfo);
    }
}
