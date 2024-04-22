use anyhow::Result;
use once_cell::sync::Lazy;
use std::process::Command;

static DID: &str = "ph/";
static GLOBAL_PATH: &str = "ph/global";
pub static GLOBAL_INFO: Lazy<sled::Db> = Lazy::new(read_now_commit);

fn check_dir_exists(path: &str) -> Result<()> {
    let path = std::path::Path::new(path);
    if !path.exists() {
        std::fs::create_dir_all(path)?;
    }
    Ok(())
}

pub fn read_now_commit() -> sled::Db {
    check_dir_exists(DID).unwrap();
    let output = Command::new("git")
        .args(&["rev-parse", "HEAD"])
        .output()
        .unwrap();
    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        panic!("git rev-parse HEAD failed: {}", stderr);
    }
    let commit = String::from_utf8_lossy(&output.stdout).trim().to_string();
    {
        let kk = format!("commit/{}", commit);
        let key: &str = kk.as_ref();
        let val: &str = commit.as_ref();
        let ph_db = sled::open(GLOBAL_PATH).unwrap();
        ph_db.insert(key, val).unwrap();
    }
    let now_db = sled::open(DID.to_string() + &commit).unwrap();

    now_db
}
