#include <sstream>
#define SDL_MAIN_USE_CALLBACKS 1

#include <SDL3/SDL.h>
#include <SDL3/SDL_events.h>
#include <SDL3/SDL_init.h>
#include <SDL3/SDL_keyboard.h>
#include <SDL3/SDL_main.h>
#include <SDL3/SDL_pixels.h>
#include <SDL3/SDL_rect.h>
#include <SDL3/SDL_render.h>
#include <SDL3/SDL_scancode.h>
#include <SDL3/SDL_timer.h>
#include <SDL3/SDL_video.h>

#include "common/base_classes.h"
#include "vm/vm.h"

#include <cstdint>
#include <fstream>
#include <iostream>
#include <memory>
#include <span>

struct App {
    SDL_Window *window = nullptr;
    SDL_Renderer *renderer = nullptr;
    std::unique_ptr<vm::VM> vm = nullptr;
    uint64_t last_frame_time = 0;
};

bool draw_rect(vm::VM &vm, std::span<vm::Value> args, vm::Value &ret,
               void *userptr) {
    if (args.size() != 1) {
        std::cerr << "unexpected argument count: " << args.size() << '\n';
        return false;
    }

    auto pos = args[0].get_field("pos");
    auto size = args[0].get_field("size");
    if (pos.empty() || size.empty()) {
        std::cerr << "failed to get fields\n";
        return false;
    }

    SDL_FRect rect{};

    auto set_field = [](float &out, vm::Value val,
                        const std::string &name) -> bool {
        auto field = val.get_field(name);
        if (field.empty()) {
            std::cerr << "failed to get field " << name << '\n';
            return false;
        }

        auto value = field.get_float();
        if (!value) {
            std::cerr << "failed to get field's  value for " << name << '\n';
            return false;
        }

        out = (float)*value;
        return true;
    };

    if (!set_field(rect.x, pos, "x") || !set_field(rect.y, pos, "y") ||
        !set_field(rect.h, size, "y") || !set_field(rect.w, size, "x")) {
        return false;
    }

    SDL_Renderer *r = (SDL_Renderer *)userptr;
    std::stringstream log;
    log << "pos x: " << rect.x << " pos y: " << rect.y;
    std::string str = log.str();

    SDL_SetRenderDrawColor(r, 255, 255, 255, SDL_ALPHA_OPAQUE);
    SDL_RenderDebugText(r, 1, 1, str.c_str());
    SDL_RenderFillRect(r, &rect);

    return true;
}

bool get_keystate(vm::VM &vm, std::span<vm::Value> args, vm::Value &ret,
                  void *userptr) {
    const common::Type *ret_type = vm.get_type("Keystate");
    if (!ret_type) {
        std::cerr << "failed to get 'Keystate' return type\n";
        return false;
    }

    ret = vm.make_value(ret_type);

    const bool *keystate = SDL_GetKeyboardState(nullptr);
    bool left = keystate[SDL_SCANCODE_A] || keystate[SDL_SCANCODE_LEFT] ||
                keystate[SDL_SCANCODE_KP_4];
    bool right = keystate[SDL_SCANCODE_D] || keystate[SDL_SCANCODE_RIGHT] ||
                 keystate[SDL_SCANCODE_KP_6];
    bool up = keystate[SDL_SCANCODE_W] || keystate[SDL_SCANCODE_UP] ||
              keystate[SDL_SCANCODE_KP_8];
    bool down = keystate[SDL_SCANCODE_S] || keystate[SDL_SCANCODE_DOWN] ||
                keystate[SDL_SCANCODE_KP_2];

    if (!ret.get_field("right").set(right)) {
        std::cerr << "failed to set right\n";
        return false;
    } else if (!ret.get_field("left").set(left)) {
        std::cerr << "failed to set left\n";
        return false;
    } else if (!ret.get_field("up").set(up)) {
        std::cerr << "failed to set up\n";
        return false;
    } else if (!ret.get_field("down").set(down)) {
        std::cerr << "failed to set down\n";
        return false;
    }

    return true;
}

bool get_window_size(vm::VM &vm, std::span<vm::Value> args, vm::Value &ret,
                     void *userptr) {
    const common::Type *ret_type = vm.get_type("Vec2");
    if (!ret_type) {
        std::cerr << "failed to get 'Vec2' return type\n";
        return false;
    }

    ret = vm.make_value(ret_type);
    int height = 0;
    int width = 0;
    if (!SDL_GetWindowSize((SDL_Window *)userptr, &width, &height)) {
        std::cerr << "failed to get window size\n";
        return false;
    }

    if (!ret.get_field("x").set((double)width)) {
        std::cerr << "failed to set height\n";
        return false;
    } else if (!ret.get_field("y").set((double)height)) {
        std::cerr << "failed to set width\n";
        return false;
    }

    return true;
}

SDL_AppResult SDL_AppInit(void **appstate, int argc, char **argv) {
    std::ifstream in{"./script"};
    if (!in.is_open()) {
        std::cerr << "failed to open script file\n";
        return SDL_APP_FAILURE;
    }

    if (!SDL_Init(SDL_INIT_VIDEO | SDL_INIT_EVENTS)) {
        std::cerr << "failed to init SDL\n";
        return SDL_APP_FAILURE;
    }

    App *app = new App{};

    if (!SDL_CreateWindowAndRenderer("example", 640, 480, SDL_WINDOW_RESIZABLE,
                                     &app->window, &app->renderer)) {
        std::cerr << "failed to init window and renderer\n";
        return SDL_APP_FAILURE;
    }

    app->vm = vm::VM::create(in, &std::cerr);
    if (!app->vm) {
        return SDL_APP_FAILURE;
    }

    app->vm->bind_native("draw_rect", draw_rect, app->renderer);
    app->vm->bind_native("get_keystate", get_keystate);
    app->vm->bind_native("get_window_size", get_window_size, app->window);
    if (!app->vm->reset()) {
        std::cerr << "failed to reset VM: " << app->vm->get_error() << '\n';
        return SDL_APP_FAILURE;
    }

    if (argc == 2) {
        std::ofstream dump{argv[1]};
        app->vm->dump(dump);
    }

    std::string err;
    if (!app->vm->call_function("init", {}, &err, nullptr)) {
        std::cerr << "failed to init script: " << err << '\n';
        return SDL_APP_FAILURE;
    }

    app->last_frame_time = SDL_GetTicks();
    *appstate = app;

    return SDL_APP_CONTINUE;
}

SDL_AppResult SDL_AppIterate(void *appstate) {
    App *app = (App *)appstate;

    SDL_SetRenderDrawColor(app->renderer, 0, 0, 0, SDL_ALPHA_OPAQUE);
    SDL_RenderClear(app->renderer);

    std::vector<vm::Value> args = {app->vm->make_value(nullptr)};

    const common::Type *double_type = app->vm->get_type("float");
    if (!double_type) {
        std::cerr << "failed to get 'float' type\n";
        return SDL_APP_FAILURE;
    }

    args[0] = app->vm->make_value(double_type);
    uint64_t ticks = SDL_GetTicks();

    if (!args[0].set((double)(ticks - app->last_frame_time) / 1000.0)) {
        std::cerr << "failed to set delta\n";
        return SDL_APP_FAILURE;
    }
    app->last_frame_time = ticks;

    std::stringstream log;
    log << "delta: " << *args[0].get_float();
    std::string str = log.str();
    SDL_SetRenderDrawColor(app->renderer, 255, 255, 255, SDL_ALPHA_OPAQUE);
    SDL_RenderDebugText(app->renderer, 1, 16, str.c_str());

    std::string err;
    if (!app->vm->call_function("frame", args, &err, nullptr)) {
        std::cerr << "error in 'frame': " << err << '\n';
        return SDL_APP_FAILURE;
    }

    SDL_RenderPresent(app->renderer);

    return SDL_APP_CONTINUE;
}

void SDL_AppQuit(void *appstate, SDL_AppResult result) {
    if (!appstate) {
        return;
    }

    App *app = (App *)appstate;

    SDL_DestroyRenderer(app->renderer);
    SDL_DestroyWindow(app->window);

    delete app;
}

SDL_AppResult SDL_AppEvent(void *appstate, SDL_Event *event) {
    if (event->type == SDL_EVENT_QUIT) {
        return SDL_APP_SUCCESS;
    }

    return SDL_APP_CONTINUE;
}
