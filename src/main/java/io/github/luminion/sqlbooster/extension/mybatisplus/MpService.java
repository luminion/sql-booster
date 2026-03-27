package io.github.luminion.sqlbooster.extension.mybatisplus;

import com.baomidou.mybatisplus.extension.service.IService;
import io.github.luminion.sqlbooster.core.BoosterService;

/**
 * 针对 MyBatis-Plus 的扩展 Service 接口。
 * 集成了 {@link IService} 与 service 层 booster 查询门面。
 *
 * @param <T> 实体类型
 * @param <V> 默认结果类型
 */
public interface MpService<T, V> extends IService<T>, BoosterService<T, V> {
}