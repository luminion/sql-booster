package io.github.luminion.sqlbooster.extension.mybatisplus;

import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import io.github.luminion.sqlbooster.model.api.QueryParams;

import java.util.List;

/**
 * 针对 Mybatis-Plus 的 ServiceImpl的扩展.
 * <p>
 * 集成了 {@link MybatisPlusBooster} 的能力,
 * 提供增强的数据库操作能力, 包括链式查询构建、Lambda 表达式支持等.
 *
 * @param <M> Mapper 类型
 * @param <T> 实体类型
 * @param <V> VO 类型
 * @author luminion
 * @since 1.0.0
 */
public abstract class BoosterMpServiceImpl<M extends BoosterMpMapper<T, V>, T, V> extends ServiceImpl<M, T> implements BoosterMpService<T, V> {

    @Override
    public List<V> selectByBooster(QueryParams<T> queryParams, Object page) {
        return getBaseMapper().selectByBooster(queryParams, page);
    }

}
