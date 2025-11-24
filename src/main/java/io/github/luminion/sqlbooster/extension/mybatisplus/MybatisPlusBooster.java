package io.github.luminion.sqlbooster.extension.mybatisplus;

import com.baomidou.mybatisplus.extension.plugins.pagination.PageDTO;
import io.github.luminion.sqlbooster.core.BoosterEngine;
import io.github.luminion.sqlbooster.core.BoosterPage;
import io.github.luminion.sqlbooster.model.api.Wrapper;
import io.github.luminion.sqlbooster.model.sql.helper.BaseHelper;
import io.github.luminion.sqlbooster.model.sql.helper.SqlHelper;
import io.github.luminion.sqlbooster.model.sql.helper.processor.SuffixProcessor;

import java.util.List;

/**
 * 针对 Mybatis-Plus 的 BoosterEngine 扩展接口. 实现分页方法
 * <p>
 * 集成了 {@link BoosterEngine} 的能力,
 * 提供增强的数据库操作能力, 包括链式查询构建、Lambda 表达式支持等.
 *
 * @param <T> 实体类型
 * @param <V> VO 类型
 * @author luminion
 * @since 1.0.0
 */
public interface MybatisPlusBooster<T, V> extends BoosterEngine<T, V> {

    @Override
    default BoosterPage<V> voPage(Wrapper<T> wrapper, long pageNum, long pageSize) {
        voPreProcess(wrapper);

        BaseHelper<T> sqlHelper = SqlHelper.of(wrapper).entity(this).process(SuffixProcessor.of()::process);
        PageDTO<V> pageInfo = new PageDTO<>(pageNum, pageSize);
        List<V> vs = selectByBooster(sqlHelper, pageInfo);
        pageInfo.setRecords(vs);
        MybatisPlusPage<V> page = new MybatisPlusPage<>(pageInfo);
        
        voPostProcess(page.getRecords(), sqlHelper, page);
        return page;
    }

}
