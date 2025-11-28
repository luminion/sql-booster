package io.github.luminion.sqlbooster.extension.mybatisplus;

import com.baomidou.mybatisplus.extension.plugins.pagination.PageDTO;
import io.github.luminion.sqlbooster.core.BoosterSupport;
import io.github.luminion.sqlbooster.model.BoosterPage;
import io.github.luminion.sqlbooster.model.SqlContext;

import java.util.List;

/**
 * @author luminion
 * @since 1.0.0
 */
public interface MybatisPlusBoosterSupport<T, V> extends BoosterSupport<T, V> {
    @Override
    default BoosterPage<V> voPage(SqlContext<T> sqlContext, long pageNum, long pageSize) {
        PageDTO<V> pageInfo = new PageDTO<>(pageNum, pageSize);
        List<V> vs = selectByBooster(sqlContext, pageInfo);
        pageInfo.setRecords(vs);
        return new MybatisPlusBoosterPage<>(pageInfo);
    }
}
