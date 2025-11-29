package io.github.luminion.sqlbooster.extension.mybatisplus;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import io.github.luminion.sqlbooster.extension.mybatis.BoosterMapper;

/**
 * 针对 Mybatis-Plus 的扩展 Mapper 接口。
 * <p>
 * 集成了 {@link BaseMapper} 和 sql-booster 的查询能力。
 */
public interface MpMapper<T, V> extends BaseMapper<T>, MpBooster<T, V>, BoosterMapper<T, V> {
}
