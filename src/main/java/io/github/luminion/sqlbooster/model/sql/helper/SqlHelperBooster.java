package io.github.luminion.sqlbooster.model.sql.helper;

import io.github.luminion.sqlbooster.core.BoosterCore;
import io.github.luminion.sqlbooster.core.BoosterPage;
import io.github.luminion.sqlbooster.model.api.Wrapper;

import java.util.List;
import java.util.Optional;

/**
 * 具备扩展查询功能的 SQL 构建助手.
 * <p>
 * 封装了 {@link BoosterCore} 和 {@link BaseHelper}, 提供了方便的链式调用查询方法.
 *
 * @param <T> 实体类型
 * @param <V> VO 类型
 * @author luminion
 * @since 1.0.0
 */
public class SqlHelperBooster<T, V> {
    /**
     * Booster 核心实例
     */
    private final BoosterCore<T, V> boosterCore;
    /**
     * 查询条件
     */
    private final Wrapper<T> wrapper;

    /**
     * 构造一个新的 {@link SqlHelperBooster} 实例.
     *
     * @param boosterCore {@link BoosterCore} 实例
     * @param wrapper     {@link Wrapper} 实例
     * @since 1.0.0
     */
    public SqlHelperBooster(BoosterCore<T, V> boosterCore, Wrapper<T> wrapper){
        this.boosterCore = boosterCore;
        this.wrapper = wrapper;
    }
    
    /**
     * 构造一个新的 {@link SqlHelperBooster} 实例.
     *
     * @param boosterCore {@link BoosterCore} 实例
     * @since 1.0.0
     */
    public SqlHelperBooster(BoosterCore<T, V> boosterCore) {
        this.wrapper = SqlHelper.of(boosterCore);
        this.boosterCore = boosterCore;
    }

    /**
     * 查询并返回结果列表中的第一个 VO 对象.
     *
     * @return VO 对象, 如果不存在则返回 null
     * @since 1.0.0
     */
    public V first() {
        return boosterCore.voFirst(this.wrapper);
    }

    /**
     * 查询并返回结果列表中的第一个 VO 对象, 并转换为指定类型.
     *
     * @param targetType 目标 VO 类型
     * @return 转换后的 VO 对象, 如果不存在则返回 null
     * @since 1.0.0
     */
    public V first(Class<V> targetType) {
        return boosterCore.voFirst(this.wrapper, targetType);
    }

    /**
     * 查询并返回一个包含第一个 VO 对象的 {@link Optional}.
     *
     * @return 包含 VO 对象的 Optional, 如果不存在则为空
     * @since 1.0.0
     */
    public Optional<V> firstOpt() {
        return boosterCore.voFirstOpt(this.wrapper);
    }

    /**
     * 查询并返回唯一的 VO 对象.
     *
     * @return VO 对象, 如果不存在则返回 null
     * @since 1.0.0
     */
    public V unique() {
        return boosterCore.voUnique(this.wrapper);
    }

    /**
     * 查询并返回唯一的 VO 对象, 并转换为指定类型.
     *
     * @param targetType 目标 VO 类型
     * @return 转换后的 VO 对象, 如果不存在则返回 null
     * @since 1.0.0
     */
    public V unique(Class<V> targetType) {
        return boosterCore.voUnique(this.wrapper, targetType);
    }

    /**
     * 查询并返回一个包含唯一 VO 对象的 {@link Optional}.
     *
     * @return 包含 VO 对象的 Optional, 如果不存在则为空
     * @since 1.0.0
     */
    public Optional<V> uniqueOpt() {
        return boosterCore.voUniqueOpt(this.wrapper);
    }

    /**
     * 查询并返回 VO 对象列表.
     *
     * @return VO 对象列表
     * @since 1.0.0
     */
    public List<V> list() {
        return boosterCore.voList(this.wrapper);
    }

    /**
     * 查询并返回指定类型的 VO 对象列表.
     *
     * @param targetType 目标 VO 类型
     * @return 转换后的 VO 对象列表
     * @since 1.0.0
     */
    public <R> List<R> list(Class<R> targetType) {
        return boosterCore.voList(this.wrapper, targetType);
    }

    /**
     * 分页查询 VO 对象.
     *
     * @param pageNum  当前页码
     * @param pageSize 每页大小
     * @return 分页结果对象
     * @since 1.0.0
     */
    public BoosterPage<V> page(int pageNum, int pageSize) {
        return boosterCore.voPage(this.wrapper, pageNum, pageSize);
    }

    /**
     * 分页查询 VO 对象.
     *
     * @param pageNum  当前页码
     * @param pageSize 每页大小
     * @return 分页结果对象
     * @since 1.0.0
     */
    public BoosterPage<V> page(long pageNum, long pageSize) {
        return boosterCore.voPage(this.wrapper, pageNum, pageSize);
    }

    /**
     * 分页查询 VO 对象, 并转换为指定类型.
     *
     * @param pageNum  当前页码
     * @param pageSize 每页大小
     * @param targetType   目标 VO 类型
     * @return 分页结果对象
     * @since 1.0.0
     */
    public <R> BoosterPage<R> page(int pageNum, int pageSize, Class<R> targetType) {
        return boosterCore.voPage(this.wrapper, pageNum, pageSize, targetType);
    }

    /**
     * 分页查询 VO 对象, 并转换为指定类型.
     *
     * @param pageNum  当前页码
     * @param pageSize 每页大小
     * @param targetType   目标 VO 类型
     * @return 分页结果对象
     * @since 1.0.0
     */
    public <R> BoosterPage<R> page(long pageNum, long pageSize, Class<R> targetType) {
        return boosterCore.voPage(this.wrapper, pageNum, pageSize, targetType);
    }


}