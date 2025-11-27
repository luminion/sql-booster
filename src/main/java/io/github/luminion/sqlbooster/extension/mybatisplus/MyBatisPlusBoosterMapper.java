package io.github.luminion.sqlbooster.extension.mybatisplus;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import io.github.luminion.sqlbooster.extension.mybatis.BoosterMapper;

/**
 * 针对 Mybatis-Plus 的 BaseMapper 扩展接口.
 *
 * @author luminion
 * @since 1.0.0
 */
public interface MyBatisPlusBoosterMapper<T, V> extends BaseMapper<T>, MybatisPlusBoosterEngine<T, V>, BoosterMapper<T, V> {
}
