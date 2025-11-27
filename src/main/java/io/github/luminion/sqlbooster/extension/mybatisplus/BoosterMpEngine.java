package io.github.luminion.sqlbooster.extension.mybatisplus;

import com.baomidou.mybatisplus.extension.plugins.pagination.PageDTO;
import io.github.luminion.sqlbooster.core.BoosterEngine;
import io.github.luminion.sqlbooster.core.BoosterPage;
import io.github.luminion.sqlbooster.core.QueryParam;

import java.util.List;

/**
 * @author luminion
 * @since 1.0.0
 */
public interface BoosterMpEngine<T, V> extends BoosterEngine<T, V> {
    @Override
    default BoosterPage<V> voPage(QueryParam<T> queryParam, long pageNum, long pageSize) {
        PageDTO<V> pageInfo = new PageDTO<>(pageNum, pageSize);
        List<V> vs = selectByBooster(queryParam, pageInfo);
        pageInfo.setRecords(vs);
        return new MybatisPlusPage<>(pageInfo);
    }
}
