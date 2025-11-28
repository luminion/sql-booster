package io.github.luminion.sqlbooster.extension.pagehelper;

import com.github.pagehelper.PageHelper;
import com.github.pagehelper.PageInfo;
import io.github.luminion.sqlbooster.core.BoosterSupport;
import io.github.luminion.sqlbooster.model.BoosterPage;
import io.github.luminion.sqlbooster.model.SqlContext;

/**
 * 针对 PageHelper 的 BoosterSupport 扩展接口.
 * <p>
 * 集成了 {@link BoosterSupport} 的能力, 提供基于 PageHelper 的分页查询功能.
 *
 * @param <T> 实体类型
 * @param <V> VO 类型
 * @author luminion
 * @since 1.0.0
 */
public interface PageHelperBoosterSupport<T, V> extends BoosterSupport<T, V> {

    @Override
    default BoosterPage<V> voPage(SqlContext<T> sqlContext, long pageNum, long pageSize) {
        PageInfo<V> pageInfo = PageHelper.startPage((int) pageNum, (int) pageSize)
                .doSelectPageInfo(() -> selectByBooster(sqlContext, null));
        return new PageHelperBoosterPage<>(pageInfo);
    }
}