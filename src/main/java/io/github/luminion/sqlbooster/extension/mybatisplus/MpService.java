package io.github.luminion.sqlbooster.extension.mybatisplus;

import com.baomidou.mybatisplus.extension.service.IService;

/**
 * 针对MyBatis-Plus 的 IService 扩展接口.
 *
 * @author luminion
 * @since 1.0.0
 */
public interface MpService<T, V> extends IService<T>, MpBooster<T, V> {
}